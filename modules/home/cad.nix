{
  lib,
  config,
  pkgs,
  ...
}:
{
  config = lib.mkIf (config.hostConfig.feature.workstation) {
    home.packages =
      with pkgs;
      [
        freecad
        openscad-unstable
        bleeding.prusa-slicer
      ];
  };
}
