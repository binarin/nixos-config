{ flake, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  inventoryHostName = "furfur";

  imports = [
    self.nixosModules.default
    ./configuration.nix
  ];

  hostConfig.features = [
    "gui"
    "wsl"
  ];
}
