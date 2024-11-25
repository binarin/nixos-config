{
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf config.hostConfig.feature.wayland {
    programs.foot = lib.mkIf (config.hostConfig.feature.wayland) {
      enable = true;
      settings = {
        main = {
          font = with config.stylix.fonts; "${monospace.name}:size=${toString sizes.terminal}";
          locked-title = true;
          selection-target = "both";
        };
        url = {
          osc8-underline = "always";
        };
        colors = {
          background = "000000";
          regular0 = "000000";
          regular1 = "cd0000";
          regular2 = "00cd00";
          regular3 = "cdcd00";
          regular4 = "0000cd";
          regular5 = "cd00cd";
          regular6 = "00cdcd";
          regular7 = "faebd7";
          bright0 = "404040";
          bright1 = "ff0000";
          bright2 = "00ff00";
          bright3 = "ffff00";
          bright4 = "0000ff";
          bright5 = "ff00ff";
          bright6 = "00ffff";
          bright7 = "ffffff";
        };
      };
    };
  };
}
