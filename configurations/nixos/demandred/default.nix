{ flake, config, ... }:
{
  imports = [
    flake.inputs.self.nixosModules.default
    ./configuration.nix
  ];

  inventoryHostName = "demandred";

  hostConfig.managedUsers = [ "binarin" ];

  hostConfig.features = [
    "hyprland"
    "interactive-cli"
    "impermanence"
  ];

}
