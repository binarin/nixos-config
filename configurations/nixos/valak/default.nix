{ flake, config, ... }:
{
  imports = [
    flake.inputs.self.nixosModules.default
    ./configuration.nix
  ];

  inventoryHostName = "valak";

  hostConfig.managedUsers = [ "binarin" ];
  hostConfig.features = [
    "hyprland"
    "lnxlink"
    "bleeding"
    "nix-builder"
    "impermanence"
  ];

}
