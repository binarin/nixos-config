{
  self,
  lib,
  config,
  ...
}:
let
  helper = import ../helpers/networks-lookup.nix { inherit self lib; };
  networks = helper.readRawInventory;
  networksLookup = helper.buildHostLookupTable networks;
  getNetworkInfo =
    netName:
    let
      net = networks."${netName}";
    in
    if net ? "info" then net.info else throw "Network ${netName} is missing `info` attribute";
  ipAllocation = lib.mapAttrsRecursiveCond (as: !(as ? "_type")) (
    path: x:
    let
      netName = lib.elemAt path 1;
      netInfo = getNetworkInfo netName;
    in
    {
      inherit (x) address;
      addressWithPrefix = "${x.address}/${builtins.toString netInfo.prefix}";
    }
  ) networksLookup;

  inventoryNetworks = lib.mapAttrs (
    _netName:
    {
      info,
      ipam ? null,
      ...
    }:
    lib.foldr lib.mergeAttrs { } [
      (lib.optionalAttrs (info ? "dns") {
        inherit (info) dns;
      })
      (lib.optionalAttrs (info ? "gateway") {
        inherit (info) gateway;
      })
      (lib.optionalAttrs (ipam != null) {
        hosts =
          with lib;
          pipe ipam [
            helper.normalizeIpam
            (lib.mapAttrs (_: (helper.taggedHostnames info)))
          ];
      })
    ]
  ) networks;
in
{
  options = {
    # inventory.ipAllocation.<HOST>.<NETWORK>.<TAG>.address - "<IP>"
    # inventory.ipAllocation.<HOST>.<NETWORK>.<TAG>.addressWithPrefix - "<IP>/<PREFIX>"
    inventory.ipAllocation = lib.mkOption {
      type = lib.types.raw;
      default = ipAllocation;
    };
    inventory.networks = lib.mkOption {
      type = lib.types.raw;
      default = inventoryNetworks;
    };
  };

  config = {
    flake.modules.generic.inventory-legacy =
      { lib, ... }:
      {
        key = "nixos-config.generic.inventory-legacy";
        options = {
          inventoryHostName = lib.mkOption { type = lib.types.str; };
          inventory.ipAllocation = lib.mkOption {
            type = lib.types.raw;
            default = ipAllocation;
          };
          inventory.networks = lib.mkOption {
            type = lib.types.raw;
            default = inventoryNetworks;
          };
        };
      };

    flake.nixosModules.inventory-legacy =
      { config, ... }:
      {
        key = "nixos-config.inventory-legacy";
        imports = [
          self.modules.generic.inventory-legacy
        ];
        config = {
          home-manager.sharedModules = [
            self.modules.generic.inventory-legacy
          ];
        };
      };
  };
}
