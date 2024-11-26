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
        prusa-slicer
      ];
      # ++ lib.optional (!config.hostConfig.feature.fast-rebuild) pkgs.bleeding.prusa-slicer;
  };
}
