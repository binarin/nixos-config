{flake, config, pkgs, lib, ...}:
let
  out-u4025qw = "Dell Inc. DELL U4025QW J7Q6FP3";
  out-lg-dualup-left = "LG Electronics LG SDQHD 311NTQDAC572";
  out-lg-dualup-right = "LG Electronics LG SDQHD 311NTSUAC574";
  out-ishamael-edp = "Sharp Corporation 0x1516 Unknown";
  out-c49rg90 = "Samsung Electric Company C49RG9x H1AK500000";
in {
  config = lib.mkIf (config.hostConfig.feature.hyprland) {
    home.packages = with pkgs; [ walker ];
    wayland.windowManager.hyprland = {
      enable = true;
      settings = {

        "$terminal" = "foot";
        "$menu" = "walker";
        "$fileManager" = "dolphin";
        "$mod" = "SUPER";
        "$hyper" = "SUPER SHIFT ALT CTRL";
        "$col_active" = "0xffaaffaa";
        "$col_inactive" = "0xff999999";

        exec-once = [
          "${pkgs.kwallet-pam}/libexec/pam_kwallet_init --no-startup-id"
          "protonmail-bridge -n"
          "nm-applet"
          "walker --gapplication-service"
          "[workspace 1 silent] foot --title 'SH|LOCAL' -e tmux new-session -A -s binarin"
          "[workspace 2 silent] emacs"
          "[workspace 4 silent] firefox"
          "[workspace 5 silent] sleep 5; exec thunderbird" # give protonmail-bridge time to startup
          "[workspace 5 silent; group new] telegram-desktop"
        ];

        general = {
          gaps_in = 3;
          gaps_out = 5;
          border_size = 2;
          resize_on_border = true;
          layout = "master";
        };

        group = {
        };

        xwayland = {
          force_zero_scaling = true;
        };

        misc = {
          force_default_wallpaper = 0;
          disable_hyprland_logo = true;
        };

        decoration = {
          rounding = 10;
          active_opacity = 1.0;
          inactive_opacity = 1.0;
          shadow = {
              enabled = true;
              range = 4;
              render_power = 3;
              # color = rgba(1a1a1aee);
          };
          blur = {
              enabled = true;
              size = 3;
              passes = 1;
              vibrancy = 0.1696;
          };
        };

        animations = {
          enabled = true;
          bezier = [
            "easeOutQuint,0.23,1,0.32,1"
            "easeInOutCubic,0.65,0.05,0.36,1"
            "linear,0,0,1,1"
            "almostLinear,0.5,0.5,0.75,1.0"
            "quick,0.15,0,0.1,1"
          ];
          animation = [
            "global, 1, 10, default"
            "border, 1, 5.39, easeOutQuint"
            "windows, 1, 4.79, easeOutQuint"
            "windowsIn, 1, 4.1, easeOutQuint, popin 87%"
            "windowsOut, 1, 1.49, linear, popin 87%"
            "fadeIn, 1, 1.73, almostLinear"
            "fadeOut, 1, 1.46, almostLinear"
            "fade, 1, 3.03, quick"
            "layers, 1, 3.81, easeOutQuint"
            "layersIn, 1, 4, easeOutQuint, fade"
            "layersOut, 1, 1.5, linear, fade"
            "fadeLayersIn, 1, 1.79, almostLinear"
            "fadeLayersOut, 1, 1.39, almostLinear"
            "workspaces, 1, 1.94, almostLinear, fade"
            "workspacesIn, 1, 1.21, almostLinear, fade"
            "workspacesOut, 1, 1.94, almostLinear, fade"
          ];
        };

        input = {
          kb_layout = "us,ru";
          kb_variant = ",winkeys";
          kb_options = "grp:menu_toggle,ctrl:nocaps,altwin:super_win,grp:sclk_toggle,compose:pause";
        };

        workspace = [
          "1, persistent:true, monitor:desc:${out-u4025qw}, layoutopt:orientation:center, default:true"
          "2, persistent:true, monitor:desc:${out-u4025qw}, layoutopt:orientation:center"
          "3, persistent:true, monitor:desc:${out-u4025qw}, layoutopt:orientation:center"
          "4, persistent:true, monitor:desc:${out-u4025qw}, layoutopt:orientation:center"
          "5, persistent:true, monitor:desc:${out-u4025qw}, layoutopt:orientation:center"

          "6, persistent:true, monitor:desc:${out-lg-dualup-left}, default:true"
          "7, persistent:true, monitor:desc:${out-lg-dualup-left}"

          "8, persistent:true, monitor:desc:${out-lg-dualup-right}, default:true"
          "9, persistent:true, monitor:desc:${out-lg-dualup-right}"
        ];

        master = {
          always_center_master = true;
          new_status = "inherited";
        };

        windowrulev2 = [
          "float, title:^(FAST_CHOICE)$"
          "center, title:^(FAST_CHOICE)$"

          # noanim isn't necessary but animations with these rules might look bad. use at your own discretion.
          "noanim, class:^(flameshot)$"
          "float, class:^(flameshot)$"
          "move 0 0, class:^(flameshot)$"
          "pin, class:^(flameshot)$"

          # set this to your leftmost monitor id, otherwise you have to move your cursor to the leftmost monitor
          # before executing flameshot
          "monitor 2, class:^(flameshot)$"
          # "suppressevent fullscreen,title:^(flameshot)"
        ];

        bind = [
          "$mod SHIFT , 0, movetoworkspace, 10"
          "$mod       , 0, workspace, 10"
          "$mod SHIFT , 1, movetoworkspace, 1"
          "$mod       , 1, workspace, 1"
          "$mod SHIFT , 2, movetoworkspace, 2"
          "$mod       , 2, workspace, 2"
          "$mod SHIFT , 3, movetoworkspace, 3"
          "$mod       , 3, workspace, 3"
          "$mod SHIFT , 4, movetoworkspace, 4"
          "$mod       , 4, workspace, 4"
          "$mod SHIFT , 5, movetoworkspace, 5"
          "$mod       , 5, workspace, 5"
          "$mod SHIFT , 6, movetoworkspace, 6"
          "$mod       , 6, workspace, 6"
          "$mod SHIFT , 7, movetoworkspace, 7"
          "$mod       , 7, workspace, 7"
          "$mod SHIFT , 8, movetoworkspace, 8"
          "$mod       , 8, workspace, 8"
          "$mod SHIFT , 9, movetoworkspace, 9"
          "$mod       , 9, workspace, 9"
          "$mod SHIFT , C, killactive"
          "$hyper     , C, layoutmsg, orientationcenter"
          "$mod       , D, exec, $menu"
          "$mod       , E, exec, $fileManager"
          "$mod       , F, fullscreen, 0"
          "$mod CTRL  , G, togglegroup"
          "$mod       , J, changegroupactive, f"
          "$mod       , K, changegroupactive, b"
          "$mod       , M, layoutmsg, swapwithmaster"
          "$mod       , N, exec, swaync-client -t"
          "$mod       , P, exec, XDG_CURRENT_DESKTOP=sway flameshot gui"
          "$mod SHIFT , Q, exit"
          "$mod SHIFT , S, movetoworkspace, special:magic"
          "$mod       , S, togglespecialworkspace, magic"
          "$mod       , V, togglefloating,"
          "$mod       , down, movefocus, d"
          "$mod SHIFT , down, movewindoworgroup, d"
          "$mod       , left, movefocus, l"
          "$mod SHIFT , left, movewindoworgroup, l"
          "$mod       , mouse_down, workspace, e+1"
          "$mod       , mouse_up, workspace, e-1"
          "$mod       , return, exec, $terminal"
          "$mod       , right, movefocus, r"
          "$mod SHIFT , right, movewindoworgroup, r"
          "$mod       , up, movefocus, u"
          "$mod SHIFT , up, movewindoworgroup, u"
          "$mod       , semicolon, exec, sshmenu"
        ];
        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];
      };
    };
  };
}
