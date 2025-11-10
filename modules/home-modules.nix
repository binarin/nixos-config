{
  lib,
  # , self
  flake-parts-lib,
  moduleLocation,
  ...
}:
let
  inherit (lib) mapAttrs mkOption types;
  inherit (flake-parts-lib) mkSubmoduleOptions;
in
{
  options = {
    flake = mkSubmoduleOptions {
      homeModules = mkOption {
        type = types.lazyAttrsOf types.unspecified;
        default = { };
        apply = mapAttrs (
          k: v: {
            _file = "${toString moduleLocation}#homeModules.${k}";
            imports = [ v ];
          }
        );
        description = ''
          Home Manager modules.

          You may use this for reusable pieces of configuration, service modules, etc.
        '';
      };
    };
  };
}
