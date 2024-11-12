{flake, lib, config, pkgs, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;

  tr = str: val: lib.trace (str + " " + builtins.toJSON val) val;

  networks = let
    val = {
      home = import "${self}/inventory/networks/home.nix";
    };
  in # tr "networks"
    val;

  reverseLookupTable = let
    val = lib.mapAttrs
      (netName: {ipam, ...}:
        let
          hostToIp = lib.mapAttrs' (ip: host: lib.nameValuePair host ip) ipam;
        in
          if lib.length (lib.attrNames hostToIp) != lib.length (lib.attrNames ipam)
          then lib.throw "Non-unique hostname in network '${netName}'"
          else hostToIp
      ) networks;
  in # tr "reverseLookupTable"
    val;

  invertAttrs = attrs: let
    inverted = lib.mapAttrs' (name: value: lib.nameValuePair value name) attrs;
  in
    if lib.length (lib.attrNames inverted) != lib.length (lib.attrNames attrs)
    then lib.throw "Non-uniqueness while inverting '${lib.toJSON attrs}'"
    else inverted;

  allHosts = let
    val = lib.attrNames (lib.mergeAttrsList (lib.attrValues reverseLookupTable));
  in # tr "allHosts"
    (lib.deepSeq val val);

  getNetworkInfo = netName: networks."${netName}".info;

  networksForHost = hostName:
    let
      networks = # tr "networks for ${hostName}"
        (lib.filterAttrs (net: hosts: lib.hasAttr hostName hosts) reverseLookupTable);
      networkNames = # tr "names for ${hostName}"
        (lib.attrNames networks);
    in
      networkNames;

  lookUpIp = netName: hostName: let
    val = reverseLookupTable."${netName}"."${hostName}";
  in
    # tr "lookUp ${netName}-${hostName}"
      val;

  genAttrsMaybe = xs: f: lib.filterAttrs (n: v: v != {}) (lib.genAttrs xs f);
in
{
  options = {
    # building statically from imports
    inventory.interfaces = lib.genAttrs ["valak"] (hostName:
      lib.genAttrs (networksForHost hostName) (netName:
        let
          inherit (lib) types;
          networkInfo = getNetworkInfo netName;
          ipWithPrefix = "${lookUpIp netName hostName}/${builtins.toString networkInfo.prefix}";
          values = {
            ipWithPrefix = [types.nonEmptyStr ipWithPrefix];
            gateway = [types.nonEmptyStr networkInfo.gateway];
            dns = [(types.listOf types.nonEmptyStr) networkInfo.dns];
          };
        in lib.genAttrs (lib.attrNames values) (name:
          let
            template = values."${name}";
            type = lib.elemAt template 0;
            default = lib.elemAt template 1;
          in lib.mkOption {
            description = "${name} for host '${hostName}' in network '${netName}'";
            inherit type default;
          }
        )
      )
    );
  };
}
