{ self, ... }:
let
  selfLib = self.lib.self;
in
{
  flake.clan.modules.lets-encrypt =
    {
      lib,
      config,
      clanLib,
      ...
    }:
    let
      clanConfig = config;
    in
    {
      _class = "clan.service";
      manifest.name = "nixos-config-let-encrypt";
      manifest.readme = ''
        centrally issued and distributed let's encrypt certs (via dns-01 challenge)
      '';
      roles.server = {
        description = "will issue certs and make age-encrypted certs available via https for other machines to download";
        interface = with lib; {
          options = {
            acme = {
              email = mkOption {
                type = types.str;
                description = "let's encrypt account name";
              };
              acceptTerms = mkEnableOption "Accept let's encrypt terms";
              server = mkOption {
                type = types.str;
                default = "https://acme-staging-v02.api.letsencrypt.org/directory";
                description = "ACME server to use";
              };
            };
          };
        };

        perInstance =
          {
            instanceName,
            settings,
            machines,
            roles,
            ...
          }:
          {
            nixosModule =
              { config, pkgs, ... }:
              {
                imports = [
                  self.nixosModules.impermanence
                ];

                impermanence.persist.directories = [ "/var/lib/acme" ];

                clan.core.vars.generators.acme-dns-challenge = {
                  prompts.cloudflare-token.description = "Cloudflare 'edit zone dns' token for dns-01 challenge";
                  files.cloudflare-token = { };
                  script = ''
                    cat $prompts/cloudflare-token > $out/cloudflare-token
                  '';
                };

                systemd.tmpfiles.settings."50-per-machine-keys" = {
                  "/var/lib/acme/.encrypted-per-machine".d = {
                    user = "acme";
                    group = "acme";
                    mode = "0755";
                  };
                };

                networking.firewall.allowedTCPPorts = [
                  443
                ];

                services.nginx = {
                  enable = true;
                  virtualHosts."${config.networking.hostName}.${config.clan.core.settings.domain}" = {
                    forceSSL = true;
                    enableACME = true;
                    acmeRoot = null; # https://github.com/NixOS/nixpkgs/issues/210807
                    serverAliases = [
                      "${config.networking.hostName}.home.binarin.info"
                    ];
                    locations."/".root = "/var/lib/acme/.encrypted-per-machine";
                  };
                };

                systemd.services = lib.flip lib.concatMapAttrs roles.client.machines (
                  name:
                  { ... }:
                  let
                    ageRecepient = lib.trim (
                      clanLib.getPublicValue {
                        flake = self;
                        machine = name;
                        generator = "acme-distribution";
                        file = "encryption-pub";
                        default = null;
                      }
                    );
                  in
                  {
                    "acme-encrypt-full-${name}" = {
                      serviceConfig = {
                        Type = "oneshot";
                        User = "acme";
                      };
                      path = [ pkgs.age ];
                      script = ''
                        set -euo pipefail
                        age --encrypt -r ${lib.escapeShellArg ageRecepient} \
                          -o /var/lib/acme/.encrypted-per-machine/${name}-full.pem \
                          /var/lib/acme/${name}/full.pem
                      '';
                    };
                    "acme-order-renew-${name}".onSuccess = [
                      "acme-encrypt-full-${name}.service"
                    ];
                  }
                );

                security.acme = {
                  acceptTerms = settings.acme.acceptTerms;
                  defaults = {
                    email = settings.acme.email;
                    server = settings.acme.server;
                    dnsProvider = "cloudflare";
                    dnsResolver = "sri.ns.cloudflare.com";
                    extraLegoRenewFlags = [
                      "--reuse-key"
                    ];
                    environmentFile = pkgs.writeText "cloudflare-settings" ''
                      CLOUDFLARE_POLLING_INTERVAL=60
                      CLOUDFLARE_PROPAGATION_TIMEOUT=3600
                    '';
                    credentialFiles.CF_DNS_API_TOKEN_FILE =
                      config.clan.core.vars.generators.acme-dns-challenge.files.cloudflare-token.path;
                  };

                  certs = lib.flip lib.mapAttrs roles.client.machines (
                    name:
                    { settings, ... }:
                    {
                      inherit (settings) domain extraDomainNames;
                    }
                  );
                };
              };
          };
      };

      roles.client = {
        description = "will pull age-encrypted cert/key from server and decrypt it";

        interface = with lib; {
          options = {
            domain = mkOption {
              type = types.str;
            };
            extraDomainNames = mkOption {
              type = types.listOf types.str;
              default = [ ];
            };
            owner = mkOption {
              type = types.str;
              default = "root";
              description = "Owner of the deployed full.pem (and its stub).";
            };
            group = mkOption {
              type = types.str;
              default = "nginx";
              description = "Group of full.pem and of the /var/lib/ssl-cert directory; consumers read the cert via membership in this group.";
            };
            mode = mkOption {
              type = types.str;
              default = "0640";
              description = "Mode of the deployed full.pem.";
            };
            reloadServices = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Consumer units ordered after pull-acme-cert and reloaded (try-reload-or-restart) when the cert changes.";
            };
          };
        };

        perInstance =
          { pkgs, instanceName, settings, ... }:
          {
            nixosModule =
              { pkgs, config, lib, ... }:
              {
                imports = [
                  self.nixosModules.impermanence
                ];

                clan.core.vars.generators = {
                  acme-distribution = {
                    files.encryption-key = { };
                    files.encryption-pub.secret = false;
                    runtimeInputs = [ pkgs.age ];
                    script = ''
                      age-keygen -o $out/encryption-key
                      age-keygen -y $out/encryption-key > $out/encryption-pub
                    '';
                  };
                };

                users.groups.acme-puller = { };
                users.users.acme-puller = {
                  isSystemUser = true;
                  group = "acme-puller";
                  extraGroups = [ "nginx" ];
                };

                impermanence.persist.directories = [ "/var/lib/ssl-cert" ];
                systemd.tmpfiles.settings."50-per-machine-keys" = {
                  "/var/lib/ssl-cert".d = {
                    user = "acme-puller";
                    group = settings.group;
                    mode = "0750";
                  };
                };

                systemd.timers."pull-acme-cert" = {
                  wantedBy = [ "timers.target" ];
                  timerConfig = {
                    OnCalendar = "*-*-* 04:15:00";
                    Unit = "pull-acme-cert.service";
                  };
                };

                systemd.services.pull-acme-cert = {
                  requiredBy = lib.unique ([ "nginx.service" ] ++ settings.reloadServices);
                  before = lib.unique ([ "nginx.service" ] ++ settings.reloadServices);
                  path = with pkgs; [
                    curl
                    age
                    coreutils
                    openssl
                    systemd
                  ];
                  serviceConfig = {
                    WorkingDirectory = "/var/lib/ssl-cert";
                  };
                  script = ''
                    set -euo pipefail

                    changed=0

                    if [[ ! -f full.pem ]]; then
                       # temp self-signed, until the first successful download
                       openssl req -x509 -newkey rsa:4096 \
                       -keyout key.pem \
                       -out full.pem \
                       -sha256 -days 3650 -nodes \
                       -subj "/C=XX/ST=StateName/L=CityName/O=CompanyName/OU=CompanySectionName/CN=CommonNameOrHostname"
                       cat key.pem >> full.pem
                       rm key.pem
                    fi

                    if curl -fsS -o new-full.pem https://acme.clan.binarin.info/${config.networking.hostName}-full.pem; then
                      if age \
                        --decrypt \
                        --identity ${config.clan.core.vars.generators.acme-distribution.files.encryption-key.path} \
                        --output new-full-decrypted.pem new-full.pem; then
                        if ! cmp -s new-full-decrypted.pem full.pem; then
                          mv new-full-decrypted.pem full.pem
                          changed=1
                        else
                          rm -f new-full-decrypted.pem
                        fi
                      fi
                    fi
                    rm -f new-full.pem

                    # Always normalize ownership/mode, regardless of which branch ran,
                    # so both the stub and the downloaded cert are protected (the PEM
                    # holds the private key).
                    chown ${settings.owner}:${settings.group} full.pem
                    chmod ${settings.mode} full.pem
                    ${lib.optionalString (settings.reloadServices != [ ]) ''
                      if [[ $changed == 1 ]]; then
                        systemctl try-reload-or-restart ${lib.concatStringsSep " " settings.reloadServices}
                      fi
                    ''}
                  '';
                };
              };
          };
      };
    };
}
