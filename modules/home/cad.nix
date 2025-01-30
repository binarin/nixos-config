{
  lib,
  config,
  pkgs,
  ...
}:
{
  config = lib.mkIf (config.hostConfig.feature.gui) {
    home.packages =
      with pkgs;
      [
        freecad
        openscad-unstable
        bleeding.prusa-slicer
      ];
  };
}
