{
  flake,
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (flake) inputs;
  inherit (inputs) self;

  tr = str: val: lib.trace (str + " " + builtins.toJSON val) val;

  networks = let
    dir = builtins.readDir "${self}/inventory/networks";
    regularNixFiles = lib.attrNames (
      lib.filterAttrs (n: v: v == "regular" && lib.hasSuffix ".nix" n) dir
    );
    val =
      lib.map
      (
        fn: let
          netName = lib.removeSuffix ".nix" fn;
        in
          lib.nameValuePair netName (import "${self}/inventory/networks/${fn}")
      )
      regularNixFiles;
  in
    lib.listToAttrs val;

  expandIpAllocationTarget = hostNameWithTags:
    if lib.isString hostNameWithTags
    then [
      hostNameWithTags
      "primary"
    ]
    else hostNameWithTags;

  assignIp = ipAllocationTarget: ip: let
    hostName = lib.head ipAllocationTarget;
    tags = lib.tail ipAllocationTarget;
  in {
    "${hostName}" = lib.genAttrs tags (tag: {
      _type = "leaf";
      address = ip;
    });
  };

  allAssignmentsForNetwork = ipam:
    lib.map
    (
      {
        name,
        value,
      }: let
        ip = name;
        allocationTarget = expandIpAllocationTarget value;
      in
        assignIp allocationTarget ip
    )
    (lib.attrsToList ipam);

  onlyKey = attrs: let
    keys = lib.attrNames attrs;
    only = lib.head keys;
  in
    if lib.length keys == 1
    then only
    else throw "Should be a singleton attrSet, but has ${builtins.toJSON keys}";

  mergeTagAssignments = lib.zipAttrsWith (
    name: values:
      if builtins.length values == 1
      then builtins.head values
      else throw "Tag '${name}' conflict ${builtins.toJSON values}"
  );

  mkReverseLookupTable = netName: {ipam, ...}: let
    hostLookup = lib.zipAttrsWith (_: mergeTagAssignments) (allAssignmentsForNetwork ipam);
  in
    lib.mapAttrs (hostName: tags: {"${netName}" = tags;}) hostLookup;

  networksLookup = let
    perNetwork = lib.attrValues (lib.mapAttrs mkReverseLookupTable networks);
  in
    lib.zipAttrsWith (hostName: networkTags: lib.foldr (a: b: a // b) {} networkTags) perNetwork;

  reverseLookupTable = let
    val =
      lib.mapAttrs
      (
        netName: {ipam, ...}: let
          hostToIp = lib.mapAttrs' (ip: host: lib.nameValuePair host ip) ipam;
        in
          if lib.length (lib.attrNames hostToIp) != lib.length (lib.attrNames ipam)
          then lib.throw "Non-unique hostname in network '${netName}'"
          else hostToIp
      )
      networks;
  in
    # tr "reverseLookupTable"
    val;

  getNetworkInfo = netName: let
    net = networks."${netName}";
  in
    if net ? "info"
    then net.info
    else throw "Network ${netName} is missing `info` attribute";

  genAttrsMaybe = xs: f: lib.filterAttrs (n: v: v != {}) (lib.genAttrs xs f);
in {
  # allAssignmentsForNetwork networks.home.ipam'
  options = {
    inventoryHostName = lib.mkOption {type = lib.types.str;};

    # inventory.ipAllocation.<HOST>.<NETWORK>.<TAG>.address - "<IP>"
    # inventory.ipAllocation.<HOST>.<NETWORK>.<TAG>.addressWithPrefix - "<IP>/<PREFIX>"
    inventory.ipAllocation =
      lib.mapAttrsRecursiveCond (as: !(as ? "_type"))
      (
        path: x: let
          hostName = lib.elemAt path 0;
          netName = lib.elemAt path 1;
          netInfo = getNetworkInfo netName;
          tag = lib.elemAt path 2;
          desc = "IP address allocation for host ${hostName} in network ${netName} (with tag '${tag}')";
        in {
          address = lib.mkOption {
            description = "${desc} - address";
            type = lib.types.nonEmptyStr;
            default = x.address;
            readOnly = true;
          };
          addressWithPrefix = lib.mkOption {
            description = "${desc} - addressWithPrefix";
            type = lib.types.nonEmptyStr;
            default = "${x.address}/${builtins.toString netInfo.prefix}";
            readOnly = true;
          };
        }
      )
      networksLookup;

    # inventory.networks.<NETWORK>.dns - [ "<DNS-IP>" ... ]
    # inventory.networks.<NETWORK>.gateway - "<GATEWAY-IP>"
    inventory.networks =
      lib.mapAttrs
      (
        netName: {info, ...}:
          lib.foldr lib.mergeAttrs {} [
            (lib.optionalAttrs (info ? "dns") {
              dns = lib.mkOption {
                description = "DNS servers for network ${netName}";
                type = lib.types.listOf lib.types.nonEmptyStr;
                default = info.dns;
              };
            })
            (lib.optionalAttrs (info ? "gateway") {
              gateway = lib.mkOption {
                description = "Gateway for network ${netName}";
                type = lib.types.nonEmptyStr;
                default = info.gateway;
              };
            })
          ]
      )
      networks;
  };
}
