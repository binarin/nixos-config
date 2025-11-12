{ ... }:
{
  flake.nixosModules.hypridle =
    { ... }:
    {
      programs.hyprlock.enable = true;
    };

  flake.homeModules.hypridle =
    { config, ... }:
    {
      systemd.user.services.hypridle.Unit.After = [ "graphical-session.target" ];
      systemd.user.services.hypridle.Unit.BindsTo = [ "graphical-session.target" ];

      services.hypridle = {
        enable = true;
        settings = {
          general = {
            lock_cmd = "pidof hyprlock || hyprlock"; # avoid starting multiple hyprlock instances.
            unlock_cmd = "systemctl --user restart stable-ssh-agent-socket-use-local.service";
            before_sleep_cmd = "loginctl lock-session"; # lock before suspend.
            after_sleep_cmd = "hyprctl dispatch dpms on"; # to avoid having to press a key twice to turn on the display.
          };
          listener = [
            {
              timeout = 150; # 2.5min.
              on-timeout = "brightnessctl-all 10"; # set monitor backlight to minimum, avoid 0 on OLED monitor.
              on-resume = "brightnessctl-all 100"; # monitor backlight restore.
            }

            {
              timeout = 300; # 5min
              on-timeout = "loginctl lock-session"; # lock screen when timeout has passed
            }

            {
              timeout = "330"; # 5.5min
              on-timeout = "hyprctl dispatch dpms off"; # screen off when timeout has passed
              on-resume = "hyprctl dispatch dpms on"; # screen on when activity is detected after timeout has fired.
            }
          ];
        };
      };

      programs.hyprlock = {
        enable = true;
        settings = {
          general = {
            disable_loading_bar = true;
            grace = 30;
            hide_cursor = true;
            ignore_empty_input = true;
          };

          background = [
            {
              path = "screenshot";
              blur_passes = 3;
              blur_size = 8;
            }
          ];

          input-field = with config.lib.stylix.colors; [
            {
              size = "200, 50";
              position = "0, -80";
              monitor = "";
              dots_center = true;
              fade_on_empty = false;
              outer_color = "rgb(${base03})";
              inner_color = "rgb(${base00})";
              font_color = "rgb(${base05})";
              fail_color = "rgb(${base08})";
              check_color = "rgb(${base0A})";
              outline_thickness = 5;
              placeholder_text = ''<span foreground="#${withHashtag.base06}">Password...</span>'';
              shadow_passes = 2;
            }
          ];
        };
      };
    };
}
