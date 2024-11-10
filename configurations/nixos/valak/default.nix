{ flake, ... }:
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
  ];
}
