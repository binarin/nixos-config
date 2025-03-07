{ flake, lib, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;

  dir = "${self}/packages";
  allFiles = builtins.readDir dir;

  nameToPathWithSkipped = lib.mapAttrs' (
    nm: tp:
    if tp == "regular" && lib.hasSuffix ".nix" nm then
      lib.nameValuePair (lib.removeSuffix ".nix" nm) "${dir}/${nm}"
    else if tp == "directory" && builtins.pathExists "${dir}/${nm}/default.nix" then
      lib.nameValuePair nm "${dir}/${nm}/default.nix"
    else
      lib.nameValuePair "__discard" null
  ) allFiles;

  nameToPath = builtins.removeAttrs nameToPathWithSkipped [ "__discard" ];

  overlay =
    final: prev:
    lib.genAttrs (lib.attrNames nameToPath) (
      nm:
      let
        fn = import nameToPath."${nm}";
        packageFn = if (builtins.functionArgs fn) ? "flake" then fn { inherit flake; } else fn;
      in
      final.callPackage packageFn { }
    );
in
{
  nixpkgs.overlays = [ overlay ];
}
