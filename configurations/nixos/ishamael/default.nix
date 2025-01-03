{ flake, config, ... }:
{
  imports = [
    flake.inputs.self.nixosModules.default
    ./configuration.nix
  ];

  inventoryHostName = "ishamael";
  hostConfig.managedUsers = [ "binarin" ];
  hostConfig.features = [
    "hyprland"
    "bleeding"
    "nix-builder"
    "impermanence"
  ];

}
