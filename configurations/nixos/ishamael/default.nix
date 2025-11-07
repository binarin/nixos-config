{ flake, config, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    self.nixosModules.default
    self.nixosModules.user-binarin
    ./configuration.nix
  ];

  inventoryHostName = "ishamael";

  hostConfig.features = [
    "hyprland"
    "nix-builder"
    "move-xdg"
    "interactive-cli"
    "emacs"
    "tailscale"
  ];

}
