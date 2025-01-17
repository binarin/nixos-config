{
  flake,
  lib,
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

  inventoryHostName = "nix-cache";
  hostConfig.deployHostName = config.hostConfig.ipAllocation.home.primary.address;
  hostConfig.features = [ "lxc" "nix-builder" ];
}
