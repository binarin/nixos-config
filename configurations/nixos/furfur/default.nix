{ flake, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  inventoryHostName = "furfur";

  imports = [
    self.nixosModules.default
    self.nixosModules.user-binarin
    ./configuration.nix
  ];

  hostConfig.features = [
    "gui"
    "wsl"
    "fast-rebuild"
    "nix-builder"
    "move-xdg"
  ];
}
