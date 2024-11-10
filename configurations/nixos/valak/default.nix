{ flake, ... }:
{
  imports = [
    flake.inputs.self.nixosModules.default
    ./configuration.nix
  ];

  inventoryHostName = "valak";

  hostConfig.managedUsers = [ "binarin" ];

  hostConfig.features = [
    "cad"
    "hyprland"
    "lnxlink"
    "bleeding"
  ];
}
