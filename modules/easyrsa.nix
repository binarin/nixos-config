{config, lib, pkgs, ...}:

with lib;

let
  allServers = config.services.openvpn-easyrsa.servers;

  inherit (pkgs) writeScript writeText easyrsa openvpn;

  ipp = name: "/var/lib/easyrsa/ipp-${name}.txt";
  pki = name: "/var/lib/easyrsa/${name}";
  ca = name: "${pki name}/ca.crt";
  cert = name: "${pki name}/issued/server.crt";
  key = name: "${pki name}/private/server.key";
  dh = name: "${pki name}/dh.pem";

  easyrsaJobName = name: "easyrsa-${name}";

  defaultCaOpts = {
    reqCountry = "US";
    reqProvince = "DC";
    reqCity = "Washington";
    reqOrg = "Org That Doesn't Exist";
    reqEmail = "example@example.com";
    reqOu = "Server Department";
    keySize = 2048;
    caExpire = 3650;
    certExpire = 3650;
  };

  makeEasyRSAJob = cfg: name:
    let
      caOpts = defaultCaOpts // cfg.ca;
      vars = writeText "easyrsa-${name}-vars" ''
        set_var EASYRSA                "${easyrsa}/share/easyrsa"
        set_var EASYRSA_PKI            "${pki name}"
        set_var EASYRSA_BATCH          "true"
        set_var EASYRSA_NS_SUPPORT     "yes"
        set_var EASYRSA_NS_COMMENT     "EasyRSA cert - ${name}"
        set_var EASYRSA_REQ_COUNTRY    "${caOpts.reqCountry}"
        set_var EASYRSA_REQ_PROVINCE   "${caOpts.reqProvince}"
        set_var EASYRSA_REQ_CITY       "${caOpts.reqCity}"
        set_var EASYRSA_REQ_ORG        "${caOpts.reqOrg}"
        set_var EASYRSA_REQ_EMAIL      "${caOpts.reqEmail}"
        set_var EASYRSA_REQ_OU         "${caOpts.reqOu}"
        set_var EASYRSA_KEY_SIZE       "${toString caOpts.keySize}"
        set_var EASYRSA_CA_EXPIRE      "${toString caOpts.caExpire}"
        set_var EASYRSA_CERT_EXPIRE    "${toString caOpts.certExpire}"
      '';

      script = writeScript "easyrsa-${name}-script" ''
        #!${pkgs.bash}/bin/bash
        set -euxo pipefail

        run() {
          ${easyrsa}/bin/easyrsa --vars=${vars} "$@"
        }

        ensure_client_tar() {
          local client=$1
          local dir=${pki name}/clients/$client
          local tar=$dir.tar.gz
          if [[ ! -f $tar ]] ; then
            rm -rf $dir
            mkdir -p $dir
            cp ${ca name} $dir/$client-ca.crt
            cp ${pki name}/issued/$client.crt $dir/$client.crt
            cp ${pki name}/private/$client.key $dir/$client.key
            (cd $dir && tar czvf $tar .)
            rm -rf $dir
          fi
        }
        mkdir -p /var/lib/easyrsa
        chmod 0700 /var/lib/easyrsa
        test -d ${pki name} || run init-pki
        test -f ${dh name} || run gen-dh
        test -f ${ca name} || run build-ca nopass
        test -f ${cert name} || run build-server-full server nopass
        for client in ${toString (attrNames cfg.clients)} ; do
          test -f ${pki name}/issued/$client.crt || run build-client-full $client nopass
          ensure_client_tar $client
        done
      '';
    in {
      description = "Generate a set of SSL certificates - ${name}";
      after = [ "local-fs.target" ];
      path = [ easyrsa pkgs.gnutar pkgs.gzip pkgs.gawk ];
      serviceConfig.Type = "oneshot";
      serviceConfig.ExecStart = script;
    };

  easyRsaJobs = listToAttrs (mapAttrsFlatten (name: value: nameValuePair (easyrsaJobName name) (makeEasyRSAJob value name)) allServers);

  defaultConfig = {
    port = 1194;
    proto = "udp";
    dev = "tun";
    comp-lzo = true;
    verb = 3;
    mute = 20;
    persist-key = true;
    persist-tun = true;
  };

  optionToString = name: value: if isBool value then name
                                else if isList value then concatStringsSep "\n" (map (singleValue: "${name} ${toString singleValue}") value)
                                else "${name} ${toString value}";
  renderConfig = attrs: concatStringsSep "\n" (mapAttrsToList optionToString attrs);

  clientConfigAttrs = cfg: if cfg ? "ccd" then cfg.ccd else {};
  clientConfigFile = serverName: clientName: clientCfg:
    writeText "client-config-${serverName}-${clientName}" (renderConfig (clientConfigAttrs clientCfg));

  client-config-dir = name: cfg: pkgs.stdenv.mkDerivation {
    name = "client-config-dir-${name}";
    buildCommand = ''
      mkdir -p $out/ccd

      ${concatStringsSep "\n" (mapAttrsToList (clientName: clientConfig: "ln -s ${clientConfigFile name clientName clientConfig} $out/ccd/${clientName}") cfg.clients)}
    '';
  };

  makeOpenVPNJob = cfg: name:
    let
      sslOpts = {
        ca = ca name;
        cert = cert name;
        key = key name;
        dh = dh name;
      };
      otherOpts = {
        port = cfg.port;
        ifconfig-pool-persist = ipp name;
        client-config-dir = "${client-config-dir name cfg}/ccd";
      };
      configValues = defaultConfig // cfg.config // sslOpts // otherOpts;
      configFile = writeText "openvpn-easyrsa-${name}.config" (renderConfig configValues);
    in {
      description = "OpenVPN/easyrsa server instance '${name}'";
      after = [ "network.target" "${easyrsaJobName name}.service" ];
      requires = [ "${easyrsaJobName name}.service" ];
      wantedBy = [ "multi-user.target" ];

      path = [ pkgs.iptables pkgs.iproute pkgs.nettools ];

      serviceConfig.ExecStart = "@${openvpn}/sbin/openvpn openvpn --config ${configFile}";
      serviceConfig.Restart = "always";
      serviceConfig.Type = "notify";
    };
  openVPNJobs = listToAttrs (mapAttrsFlatten (name: value: nameValuePair "openvpn-easyrsa-${name}" (makeOpenVPNJob value name)) allServers);

  udpServers = filterAttrs (n: cfg: cfg.proto == "udp") allServers;
  tcpServers = filterAttrs (n: cfg: cfg.proto == "tcp") allServers;

in {
  options = {
    services.openvpn-easyrsa.servers = mkOption {
      default = {};
      type = types.unspecified;
    };
  };

  config = mkIf (allServers != {}) {
    systemd.services = easyRsaJobs // openVPNJobs;
    environment.systemPackages = [ pkgs.easyrsa pkgs.openvpn ];
    networking.firewall.allowedUDPPorts = mapAttrsToList (n: cfg: cfg.port) udpServers;
    networking.firewall.allowedTCPPorts = mapAttrsToList (n: cfg: cfg.port) tcpServers;
  };
}
