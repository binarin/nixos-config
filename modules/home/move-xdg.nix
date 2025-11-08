{ config, lib, ... }:
{
  options.xdg =
    with lib;
    with types;
    {
      cacheHomeRelative = mkOption {
        type = str;
        default = ".cache";
      };
      configHomeRelative = mkOption {
        type = str;
        default = ".config";
      };
      dataHomeRelative = mkOption {
        type = str;
        default = ".local/share";
      };
      stateHomeRelative = mkOption {
        type = str;
        default = ".local/state";
      };
    };
  config = lib.mkIf config.hostConfig.feature.move-xdg {
    xdg = {
      enable = true;
      cacheHomeRelative = ".xdg/cache";
      configHomeRelative = ".xdg/config";
      dataHomeRelative = ".xdg/local/share";
      stateHomeRelative = ".xdg/local/state";
    };
  };
}
