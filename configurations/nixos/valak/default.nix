{ flake, config, ... }:
{
  imports = [
    flake.inputs.self.nixosModules.default
    ./configuration.nix
  ];

  inventoryHostName = "valak";

  hostConfig.managedUsers = [ "binarin" ];

  hostConfig.features = [
    "vfio"
    "hyprland"
    "lnxlink"
    "nix-builder"
    "impermanence"
  ];

}
