# A module that automatically imports everything else in the parent folder.
{ flake, lib, ... }:
{
  imports = [ flake.inputs.self.sharedModules.default ] ++
    (with builtins;
    map
      (fn: ./${fn})
      (filter (fn: fn != "default.nix") (attrNames (readDir ./.))));
}
