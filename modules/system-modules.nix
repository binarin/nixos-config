{
  lib,
  flake-parts-lib,
  moduleLocation,
  ...
}:
let
  inherit (lib) mapAttrs mkOption types;
in
{
  options = {
    flake = flake-parts-lib.mkSubmoduleOptions {
      systemConfigs = mkOption {
        type = types.lazyAttrsOf types.raw;
        default = { };
        description = ''
          Instantiated system-manager configurations.
        '';
      };
      systemModules = mkOption {
        type = types.lazyAttrsOf types.deferredModule;
        default = { };
        apply = mapAttrs (
          k: v: {
            _file = "${toString moduleLocation}#systemModules.${k}";
            imports = [ v ];
          }
        );
        description = ''
          System-manager modules.

          Reusable pieces of system configuration for non-NixOS Linux hosts.
        '';
      };
    };
  };
}
