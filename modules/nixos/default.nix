{ flake, lib, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in {
  imports = [
    self.sharedModules.default
    inputs.arion.nixosModules.arion
    self.nixosModules.nix
    self.nixosModules.inventory-legacy
  ] ++ (with builtins; map (fn: ./${fn}) (filter (fn: fn != "default.nix") (attrNames (readDir ./.))));
}
