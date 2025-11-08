{
  lib,
  config,
  pkgs,
  ...
}:
let
  freecad-dpi-wrapper =
    pkgs.runCommand "FreeCAD-dpi-aware"
      {
      }
      ''
        mkdir -p $out/bin $out/share

        cat <<'EOF' > $out/bin/FreeCAD
        #!${pkgs.runtimeShell}
        HYPRCTL=${lib.getExe' config.wayland.windowManager.hyprland.package "hyprctl"}
        JQ=${lib.getExe pkgs.jq}
        SCALE=$($HYPRCTL monitors -j | $JQ ".[0].scale")
        export QT_SCALE_FACTOR=$SCALE
        exec ${lib.getExe' pkgs.freecad "FreeCAD"} "$@"
        EOF
        chmod +x $out/bin/FreeCAD

        ln -s "${lib.getExe' pkgs.freecad "FreeCADCmd"}" $out/bin/
        ln -s ${pkgs.freecad}/share/applications $out/share/
        ln -s ${pkgs.freecad}/share/icons $out/share/
      '';
in
{
  config = lib.mkIf (config.hostConfig.feature.workstation) {
    home.packages = with pkgs; [
      freecad-dpi-wrapper
      openscad-unstable
      prusa-slicer
    ];
  };
}
