# See /modules/nixos/* for actual settings
# This file is just *top-level* configuration.
{
  flake,
  config,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    self.nixosModules.default
    ./configuration.nix
  ];

  inventoryHostName = "monitor";
  hostConfig.deployHostName = config.hostConfig.ipAllocation.home.primary.address;
  hostConfig.features = [ "lxc" ];
}
