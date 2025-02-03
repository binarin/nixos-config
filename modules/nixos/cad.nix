{
  lib,
  config,
  pkgs,
  ...
}:
{
  config = lib.mkIf (config.hostConfig.feature.workstation) {
    hardware.spacenavd.enable = true;
  };
}
