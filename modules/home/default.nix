# A module that automatically imports everything else in the parent folder.
{lib, ...}:
{
  imports =
    with builtins;
    map
      (fn: ./${fn})
      (filter (fn: fn != "default.nix") (attrNames (readDir ./.)));

  config._module.args.pkgsPath = <nixpkgs>;
}
