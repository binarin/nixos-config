{
  lib,
  config,
  pkgs,
  ...
}:
{
  config = lib.mkIf (config.hostConfig.feature.gui) {
    hardware.spacenavd.enable = true;
  };
}
