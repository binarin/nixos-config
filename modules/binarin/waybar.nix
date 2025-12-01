{
  self,
  ...
}:
{
  flake.homeModules.waybar =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.home.waybar";
      imports = [
        self.modules.generic.zenburn
        self.modules.generic.flake-files
      ];

      options = {
        programs.waybar.battery = {
          enable = lib.mkEnableOption "battery module in waybar" // {
            default = false;
          };

          name = lib.mkOption {
            type = lib.types.str;
            default = "BAT1";
            description = "Battery name to monitor";
          };
        };
      };

      config = {
        home.packages = [ pkgs.noto-fonts ];

        systemd.user.services.waybar.Unit.After = [ "graphical-session.target" ];
        systemd.user.services.waybar.Unit.BindsTo = [ "graphical-session.target" ];
        systemd.user.services.waybar.Service.ExecCondition = [
          ''
            ${pkgs.systemd}/lib/systemd/systemd-xdg-autostart-condition "wlroots:niri:sway:Wayfire:labwc:Hyprland" ""
          ''
        ];

        services.network-manager-applet.enable = true;

        programs.waybar = {
          enable = true;
          systemd.enable = true;
          settings = {
            mainBar = {
              reload_style_on_change = true;
              position = "bottom";
              layer = "top";
              height = 21;

              modules-left = [
                "niri/workspaces"
                "wlr/taskbar"
              ];
              modules-center = [ "niri/window" ];
              modules-right = [
                "tray"
                "idle_inhibitor"
                "pulseaudio"
              ]
              ++ lib.optionals config.programs.waybar.battery.enable [ "battery" ]
              ++ [
                "clock"
                "niri/language"
                "custom/notification"
              ];

              "niri/workspaces" = {
                current-only = true;
              };

              "wlr/taskbar" = {
                on-click = "activate";
              };

              "niri/window" = {
                separate-outputs = true;
                icon = true;
                icon-size = 16;
              };

              "tray" = {
                icon-size = 16;
                spacing = 5;
              };

              "battery" = {
                bat = config.programs.waybar.battery.name;
                interval = "10";
                states = {
                  good = 95;
                  warning = 30;
                  critical = 15;
                };
                format = "{icon} {capacity}%";
                format-charging = " {capacity}%";
                format-plugged = " {capacity}%";
                format-alt = "{time} {icon}";
                format-icons = [
                  "󰂎"
                  "󰁺"
                  "󰁻"
                  "󰁼"
                  "󰁽"
                  "󰁾"
                  "󰁿"
                  "󰂀"
                  "󰂁"
                  "󰂂"
                  "󰁹"
                ];
              };

              idle_inhibitor = {
                format = "{icon}";
                format-icons = {
                  activated = "🛑";
                  deactivated = "";
                };
              };

              pulseaudio = {
                format = "{volume}% {icon}🔊";
                format-bluetooth = "{volume}% {icon}";
                format-muted = "🔇";
                format-icons = {
                  headphone = "🎧";
                  hands-free = "🎧";
                };
                scroll-step = 1;
                on-click = "pavucontrol";
              };

              clock = {
                format = "{:%a, %Y-%m-%d %H:%M:%S}";
                tooltip-format = "<tt><big>{calendar}</big></tt>";
                interval = 1;
                locale = "nl_NL.UTF-8";
                actions = {
                  on-click-right = "mode";
                  on-scroll-up = "shift_up";
                  on-scroll-down = "shift_down";
                  on-click-middle = "shift_reset";
                };
                calendar = {
                  mode = "year";
                  mode-mon-col = 3;
                  week-pos = "right";
                  on-scroll = 1;
                  format = {
                    months = "<span color='${config.zenburn.colors.cyan}'><b>{}</b></span>";
                    days = "<span color='${config.zenburn.colors.fg}'><b>{}</b></span>";
                    weeks = "<span color='${config.zenburn.colors.green}'><b>W{}</b></span>";
                    weekdays = "<span color='${config.zenburn.colors.yellow}'><b>{}</b></span>";
                    today = ''<span background="${config.zenburn.colors.orange}" color="${config.zenburn.colors.fg_minus_1}"><b><u>{}</u></b></span>'';
                  };
                };
              };

              "niri/language" = {
                format-ru = "🇷🇺";
                format-en = "🇺🇸";
                format-nl = "🇳🇱";
                format-es = "🇪🇸";
              };

              "custom/notification" = {
                tooltip = false;
                format = "{icon}";
                format-icons = {
                  notification = "<span foreground='red'><sup></sup></span>";
                  none = "";
                  dnd-notification = "<span foreground='red'><sup></sup></span>";
                  dnd-none = "";
                };
                return-type = "json";
                exec = "${pkgs.swaynotificationcenter}/bin/swaync-client -swb";
                on-click = "${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
                on-click-right = "${pkgs.swaynotificationcenter}/bin/swaync-client -d -sw";
                escape = true;
              };
            };
          };
          style = config.lib.style.template "waybar-style.css" (config.lib.self.file "waybar-style.css") { };
        };
      };
    };
}
