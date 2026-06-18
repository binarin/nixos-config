{ self, ... }:
{
  flake.systemModules.compat =
    { lib, config, ... }:
    {
      options = {
        services.graphical-desktop.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Whether graphical desktop services are enabled.";
        };

        impermanence.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Whether impermanence is enabled.";
        };

        networking.hostName = lib.mkOption {
          type = lib.types.str;
          description = "The hostname of the machine.";
        };
      };
    };
}
