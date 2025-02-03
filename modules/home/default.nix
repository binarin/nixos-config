# A module that automatically imports everything else in the parent folder.
{ flake, lib, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in {
  imports = [
    self.sharedModules.default
  ] ++ (with builtins; map (fn: ./${fn}) (filter (fn: fn != "default.nix") (attrNames (readDir ./.))));
}
