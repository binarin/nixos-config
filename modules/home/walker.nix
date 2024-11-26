{ flake, config, pkgs, lib, ... }:
{
  imports = [
    flake.inputs.walker.homeManagerModules.default
  ];

  options = {
  };

  config = lib.mkIf config.hostConfig.feature.hyprland {
    home.packages = [ pkgs.walker ];
    programs.walker = {
      enable = true;
      package = pkgs.walker;
      runAsService = true;

      # All options from the config.json can be used here.
      config = {
        # search.placeholder = "Example";
        # ui.fullscreen = true;
        list = {
          height = 200;
        };
        websearch.prefix = "?";
        switcher.prefix = "/";
      };

      # If this is not set the default styling is used.
      # style = ''
      #   * {
      #     color: #dcd7ba;
      #   }
      # '';
    };
  };
}
