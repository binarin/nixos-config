{flake, pkgs, lib, config, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  home.packages = [ pkgs.noto-fonts ];
  programs.waybar = {
    enable = true;
    # systemd.enable = true;
    settings = {
      mainBar = {
        position = "bottom";
        layer = "top";
        height = 34;
        # output = [
        #   "eDP-1"
        #   "HDMI-A-1"
        # ];

        modules-left = [ "hyprland/workspaces" "wlr/taskbar" ];
        modules-center = [ "hyprland/window" ];
        modules-right = [ "tray" "idle_inhibitor" "pulseaudio" "clock" "custom/notification" ];

        "custom/notification" = {
          tooltip = false;
          format = "{icon}";
          format-icons = {
            notification = "<span foreground='red'><sup></sup></span>";
            none = "";
            dnd-notification = "<span foreground='red'><sup></sup></span>";
            dnd-none = "";
          };
          return-type = "json";
          # exec-if = "which swaync-client";
          exec = "${pkgs.swaynotificationcenter}/bin/swaync-client -swb";
          on-click = "${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
          on-click-right = "${pkgs.swaynotificationcenter}/bin/swaync-client -d -sw";
          escape = true;
        };

        "tray" = {
          icon-size = 32;
        };

        "hyprland/workspaces" = { };

        "hyprland/window" = {
          separate-outputs = true;
        };

        "wlr/taskbar" = {
          on-click = "activate";
        };

        clock = {
          format = "{:%a, %Y-%m-%d %H:%M:%S}";
          interval = 1;
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
          format-bluetooth = "{volume}% {icon}";
          format-muted = "🔇";
          format-icons = {
            headphone = "🎧";
            hands-free = "🎧";
          };
          scroll-step = 1;
          on-click = "pavucontrol";
        };
      };
    };
    style = pkgs.writeText "waybar-style.css" (builtins.readFile "${self}/users/waybar-style.css");
  };

  home.file.".config/waybar/base16-zenburn.css".source = pkgs.writeText "base16-zenburn.css" (builtins.readFile "${self}/users/base16-zenburn.css");
}
