{ flake, ... }:
{
  imports = [
    flake.inputs.self.nixosModules.default
    ./configuration.nix
  ];

  inventoryHostName = "valak";

  hostConfig.features = [
    "hyprland"
    "lnxlink"
    "bleeding"
  ];
}
