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
    "nix-builder"
    "move-xdg"
    "interactive-cli"
    "emacs"
    "tailscale"
  ];

}
