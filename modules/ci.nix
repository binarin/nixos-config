# CI metadata module
{ ... }:
{
  config = {
    flake.nixosModules.ci =
      { lib, ... }:
      {
        key = "nixos-config.modules.nixos.ci";

        options.ci = {
          doBuild = lib.mkOption {
            type = lib.types.bool;
            default = true;
            description = "Whether this configuration should be built by CI";
          };
        };
      };
  };
}
