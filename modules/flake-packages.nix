# XXX get rid of it
{ self, lib, ... }:
let
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
    final: _prev:
    lib.genAttrs (lib.attrNames nameToPath) (
      nm:
      let
        fn = import nameToPath."${nm}";
        # Provide self as flake argument for compatibility with old packages
        packageFn =
          if (builtins.functionArgs fn) ? "flake" then
            fn {
              flake = {
                inputs = { inherit self; };
              };
            }
          else
            fn;
      in
      final.callPackage packageFn { }
    );
in
{

  flake.nixosModules.flake-packages =
    { ... }:
    {
      key = "nixos-config.modules.nixos.flake-packages";

      config = {
        nixpkgs.overlays = [ overlay ];
      };
    };
}
