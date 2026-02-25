{
  self,
  lib,
  config,
  ...
}:
let
  flakeConfig = config;
  helper = import "${self}/lib/networks-lookup.nix" { inherit self lib; };
  networks = helper.readRawInventory;
  networksLookup = helper.buildHostLookupTable networks;
  usersGroups = import "${self}/inventory/users-groups.nix";
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
      inherit (x) address mac;
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
      (lib.optionalAttrs (info ? "network") {
        inherit (info) network;
      })
      (lib.optionalAttrs (info ? "prefix") {
        inherit (info) prefix;
      })
      (lib.optionalAttrs (info ? "searchdomain") {
        inherit (info) searchdomain;
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
    inventory.usersGroups = lib.mkOption {
      type = lib.types.raw;
      default = usersGroups;
    };
  };

  config = {
    flake.modules.generic.inventory-legacy =
      { lib, ... }:
      {
        key = "nixos-config.modules.generic.inventory-legacy";
        options = {
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

    flake.nixosModules.inventory =
      { config, ... }:
      {
        key = "nixos-config.modules.nixos.inventory";
        options = {
          inventory.hostIpAllocation = lib.mkOption {
            type = lib.types.raw;
            readOnly = true;
          };
        };
        config = {
          networking.hostId =
            (builtins.fromTOML (builtins.readFile "${self}/inventory/host-id.toml"))
            ."${config.networking.hostName}";
          networking.hosts = flakeConfig.inventory.networks.home.hosts;
          inventory.hostIpAllocation = flakeConfig.inventory.ipAllocation."${config.networking.hostName}";
        };
      };

    flake.nixosModules.inventory-legacy =
      { ... }:
      {
        key = "nixos-config.modules.nixos.inventory-legacy";
        imports = [
          self.modules.generic.inventory-legacy
        ];
      };
  };
}
