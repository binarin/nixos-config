# See /modules/nixos/* for actual settings
# This file is just *top-level* configuration.
{ flake, lib, config, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    self.nixosModules.default
    ./configuration.nix
  ];

  inventoryHostName = "media";
  hostConfig.deployHostName = config.hostConfig.ipAllocation.home.primary.address;
  hostConfig.features = [
    "lxc"
  ];
}
