{ flake, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  inventoryHostName = "furfur";

  imports = [
    self.nixosModules.default
    self.nixosModules.emacs
    ./configuration.nix
  ];
}
