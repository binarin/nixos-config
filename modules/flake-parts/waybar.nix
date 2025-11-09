{
  self,
  ...
}:
{
  flake.nixosModules.waybar =
    {
      ...
    }:
    {
      key = "nixos-config.modules.nixos.waybar";
      nixpkgs.overlays = [
        # flake.inputs.waybar.overlays.default
      ];
      home-manager.sharedModules = [ self.homeModules.waybar ];
    };

  flake.homeModules.waybar =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.home.waybar";
      imports = [ self.homeModules.zenburn ];

      config = {
        home.packages = [ pkgs.noto-fonts ];

        systemd.user.services.waybar.Unit.After = [ "graphical-session.target" ];
        systemd.user.services.waybar.Unit.BindsTo = [ "graphical-session.target" ];
        systemd.user.services.waybar.Service.ExecCondition = [
          ''
            ${pkgs.systemd}/lib/systemd/systemd-xdg-autostart-condition "wlroots:sway:Wayfire:labwc:Hyprland" ""
          ''
        ];

        programs.waybar = {
          enable = true;
          systemd.enable = true;
          settings = {
            mainBar = {
              position = "bottom";
              layer = "top";
              height = 16;

              modules-left = [
                "hyprland/workspaces"
                "wlr/taskbar"
              ];
              modules-center = [ "hyprland/window" ];
              modules-right = [
                "tray"
                "idle_inhibitor"
                "pulseaudio"
                "clock"
                "hyprland/submap"
                "hyprland/language"
                "custom/notification"
              ];

              "hyprland/workspaces" = { };

              "wlr/taskbar" = {
                on-click = "activate";
              };

              "hyprland/window" = {
                separate-outputs = true;
                icon = true;
                icon-size = 16;
              };

              "tray" = {
                icon-size = 16;
                spacing = 5;
              };

              idle_inhibitor = {
                format = "{icon}";
                format-icons = {
                  activated = "🛑";
                  deactivated = "";
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

              "hyprland/language" = {
                format-ru = "🇷🇺";
                format-en = "🇺🇸";
                format-nl = "🇳🇱";
                format-es = "🇪🇸";
              };

              "hyprland/submap" = { };

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
