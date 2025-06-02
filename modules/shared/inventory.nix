{
  flake,
  lib,
  config,
  pkgs,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;

  inherit (self.helpers.networks-lookup) normalizeIpam taggedHostnames;

  networks = self.helpers.networks-lookup.readRawInventory;
  networksLookup = self.helpers.networks-lookup.buildHostLookupTable networks;

  tr = str: val: lib.trace (str + " " + builtins.toJSON val) val;

  getNetworkInfo =
    netName:
    let
      net = networks."${netName}";
    in
    if net ? "info" then net.info else throw "Network ${netName} is missing `info` attribute";

  genAttrsMaybe = xs: f: lib.filterAttrs (n: v: v != { }) (lib.genAttrs xs f);
in
{
  # allAssignmentsForNetwork networks.home.ipam'
  options = {
    inventoryHostName = lib.mkOption { type = lib.types.str; };

    # inventory.ipAllocation.<HOST>.<NETWORK>.<TAG>.address - "<IP>"
    # inventory.ipAllocation.<HOST>.<NETWORK>.<TAG>.addressWithPrefix - "<IP>/<PREFIX>"
    inventory.ipAllocation = lib.mapAttrsRecursiveCond (as: !(as ? "_type")) (
      path: x:
      let
        hostName = lib.elemAt path 0;
        netName = lib.elemAt path 1;
        netInfo = getNetworkInfo netName;
        tag = lib.elemAt path 2;
        desc = "IP address allocation for host ${hostName} in network ${netName} (with tag '${tag}')";
      in
      {
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
    ) networksLookup;

    # inventory.networks.<NETWORK>.dns - [ "<DNS-IP>" ... ]
    # inventory.networks.<NETWORK>.gateway - "<GATEWAY-IP>"
    inventory.networks = lib.mapAttrs (
      netName:
      { info, ipam ? null, ... }:
      lib.foldr lib.mergeAttrs { } [
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
        (lib.optionalAttrs (ipam != null) {
          hosts = with lib; mkOption {
            description = "All hosts ready to be included in /etc/hosts";
            type = types.attrsOf (types.listOf types.str);
            default = pipe ipam [
              normalizeIpam
              (lib.mapAttrs (_: (taggedHostnames info)))
            ];
          };
        })
      ]
    ) networks;
  };
}
