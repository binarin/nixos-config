# -*- nix -*-
{
  self,
  inputs,
  ...
}:
{
  flake-file.inputs = {
    hyprland = {
      url = "https://github.com/hyprwm/Hyprland";
      type = "git";
      submodules = true;
      ref = "refs/tags/v0.51.1";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    };
    hyprland-contrib = {
      url = "github:hyprwm/contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  flake.nixosModules.hyprland =
    {
      pkgs,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.hyprland";

      config = {
        nixpkgs.overlays = [
          inputs.hyprland-contrib.overlays.default
        ];

        services.displayManager.defaultSession = "hyprland-uwsm";

        security.pam.services.login.kwallet.enable = true;
        security.pam.services.login.kwallet.package = lib.mkForce pkgs.kdePackages.kwallet-pam;

        programs.hyprlock.enable = true;
        programs.hyprland.enable = true;
        programs.hyprland.package = inputs.hyprland.packages."${pkgs.stdenv.hostPlatform.system}".hyprland;
        programs.hyprland.withUWSM = true;

        home-manager.sharedModules = [
          # self.homeModules.hyprland
          # self.homeModules.waybar
        ];
      };
    };

  flake.homeModules.hyprland =
    {
      config,
      pkgs,
      lib,
      osConfig ? null,
      ...
    }:
    let
      shellevents = inputs.hyprland-contrib.packages."${pkgs.stdenv.hostPlatform.system}".shellevents;
      out-u4025qw = "Dell Inc. DELL U4025QW J7Q6FP3";
      out-lg-dualup-left = "LG Electronics LG SDQHD 311NTQDAC572";
      out-lg-dualup-right = "LG Electronics LG SDQHD 311NTSUAC574";
      out-ishamael-edp = "Sharp Corporation 0x1516 Unknown";
      out-c49rg90 = "Samsung Electric Company C49RG9x H1AK500000";
      my-shellevents = pkgs.writeScript "my-shellevents" ''
        #!${lib.getExe pkgs.bash}
        ${lib.getExe pkgs.socat} -u UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock EXEC:"${lib.getExe shellevents} ${config.lib.self.file "hyprland-shellevents.sh"}",nofork
      '';
      rgb = color: "rgb(${lib.removePrefix "#" (config.zenburn.colors."${color}")})";
      hyprlandDefaultOrientation = {
        demandred = "left";
        _default = "left";
      };
    in
    {
      key = "nixos-config.modules.home.hyprland";

      imports = [
        self.homeModules.waybar
        self.homeModules.wayland
        self.modules.generic.zenburn
      ];

      config = {
        home.sessionVariables = {
          # Fix for some Java AWT applications (e.g. Android Studio),
          # use this if they aren't displayed properly:
          "_JAVA_AWT_WM_NONREPARENTING" = "1";
        };

        xdg.portal.extraPortals = [
          pkgs.kdePackages.kwallet
          pkgs.kdePackages.xdg-desktop-portal-kde
        ];

        xdg.configFile =
          lib.mapAttrs'
            (
              name: value:
              lib.nameValuePair "autostart/${name}.desktop" {
                text = ''
                  [Desktop Entry]
                  ${value}
                '';
              }
            )
            {
              "org.kde.kalendarac" = "NotShowIn=Hyrpland";
              "org.kde.kunifiedpush-distributor" = "NotShowIn=Hyrpland";
              "org.kde.xwaylandvideobridge" = "NotShowIn=Hyrpland";
              "git-annex" = "Hiddent=True";
              "ProtonMailBridge" = "Hidden=True";
            };

        home.packages = with pkgs; [
          pkgs.kdePackages.kwalletmanager
          pkgs.kdePackages.kwallet
          fuzzel
          hyprland-per-window-layout
          hyprshot
          networkmanagerapplet
          self.packages."${pkgs.stdenv.hostPlatform.system}".sshmenu
          swaynotificationcenter
        ];

        xsession.preferStatusNotifierItems = true;

        wayland.windowManager.hyprland = {
          systemd.enable = false;
          enable = true;
          settings = {
            "$terminal" = "foot";
            "$menu" = "fuzzel --launch-prefix='uwsm app -t service --'";
            "$fileManager" = "dolphin";
            "$mod" = "SUPER";
            "$hyper" = "SUPER SHIFT ALT CTRL";
            "$col_active" = "0xffaaffaa";
            "$col_inactive" = "0xff999999";

            exec-once = [
              "${osConfig.security.pam.services.login.kwallet.package}/libexec/pam_kwallet_init"
              "uwsm app -t service -u hyprland-exec-once-hyrpland-per-window-layout.service -- hyprland-per-window-layout"
              "uwsm app -t service -u hyprland-exec-once-my-shellevents.service -- ${my-shellevents}"
              # "[workspace 1 silent] uwsm app -t scope -u hyprland-exec-once-foot.scope -- foot --title 'SH|LOCAL' -e tmux new-session -A -s binarin"
              "[workspace 1 silent] uwsm app -t scope -u hyprland-exec-once-wezterm.scope -- wezterm"
              "[workspace 2 silent] uwsm app -t scope -u hyprland-exec-once-emacs.scope -- emacs"
              "[workspace 4 silent] uwsm app -t scope -u hyprland-exec-once-firefox.scope -- firefox"
              "[workspace 5 silent] sleep 2; exec uwsm app -t scope -u hyprland-exec-once-thunderbird.scope -- thunderbird"
              "[workspace 5 silent; group new] sleep 1; uwsm app -t scope -u hyprland-exec-once-telegram-desktop.scope -- telegram-desktop"
              ''sleep 3; hyprctl --batch "dispatch workspace 1''
            ];

            # debug.disable_logs = false;

            general = {
              gaps_in = 3;
              gaps_out = 5;
              border_size = 2;
              resize_on_border = true;
              layout = "master";
              "col.inactive_border" = rgb "blue_minus_4";
              "col.active_border" = rgb "green_plus_3";
            };

            group = {
              "group_on_movetoworkspace" = true;
              "col.border_active" = rgb "red";
              "col.border_inactive" = rgb "orange";
              "col.border_locked_active" = rgb "red_minus_6";

              groupbar = {
                enabled = false;
                stacked = false;
                render_titles = false;
                height = 3;
                text_color = rgb "fg_plus_2";
                "col.inactive" = rgb "blue_minus_4";
                "col.active" = rgb "red";
              };

            };

            xwayland = {
              force_zero_scaling = true;
            };

            misc = {
              force_default_wallpaper = 0;
              disable_hyprland_logo = true;
              background_color = "0x00807F";
            };

            decoration = {
              rounding = 10;
              active_opacity = 1.0;
              inactive_opacity = 1.0;
              shadow = {
                enabled = true;
                range = 4;
                render_power = 3;
                color = rgb "bg_minus_1";
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
              kb_options = "grp:menu_toggle,ctrl:nocaps,grp:sclk_toggle";
            };

            workspace = [
              "1, persistent:true, monitor:desc:${out-u4025qw}, default:true"
              "2, persistent:true, monitor:desc:${out-u4025qw}"
              "3, persistent:true, monitor:desc:${out-u4025qw}"
              "4, persistent:true, monitor:desc:${out-u4025qw}"
              "5, persistent:true, monitor:desc:${out-u4025qw}"

              "6, persistent:true, monitor:desc:${out-lg-dualup-left}, default:true"
              "7, persistent:true, monitor:desc:${out-lg-dualup-left}"

              "8, persistent:true, monitor:desc:${out-lg-dualup-right}, default:true"
              "9, persistent:true, monitor:desc:${out-lg-dualup-right}"
            ];

            monitor =
              ({
                ishamael = [
                  # "HDMI-A-2, Modeline 950.50 5120 5536 6104 7088 2160 2161 2164 2235 -hsync +vsync, 0x0, 2.0"
                  ", preferred, auto, auto"
                ];
                demandred = [ ", preferred, auto, auto" ];
              })."${osConfig.networking.hostName}" or [ ", preferred, auto, auto" ];

            master = {
              slave_count_for_center_master = 0;
              new_status = "inherited";
              orientation =
                if hyprlandDefaultOrientation ? osConfig.networking.hostName then
                  hyprlandDefaultOrientation."${osConfig.networking.hostName}"
                else
                  hyprlandDefaultOrientation._default;
            };

            windowrulev2 = [
              "float, title:^(FAST_CHOICE)$"
              "center, title:^(FAST_CHOICE)$"

              "noinitialfocus, class:FreeCAD"

              # noanim isn't necessary but animations with these rules might look bad. use at your own discretion.
              # "noanim, class:^(flameshot)$"
              # "float, class:^(flameshot)$"
              # "move 0 0, class:^(flameshot)$"
              # "pin, class:^(flameshot)$"

              # set this to your leftmost monitor id, otherwise you have to move your cursor to the leftmost monitor
              # before executing flameshot
              # "monitor 2, class:^(flameshot)$"
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
              "$mod       , E, exec, uwsm app -t service -- $fileManager"
              "$mod       , F, fullscreen, 0"
              "$mod CTRL  , G, togglegroup"
              "$mod       , J, changegroupactive, f"
              "$mod       , K, changegroupactive, b"
              "$mod       , M, layoutmsg, swapwithmaster"
              "$mod       , N, exec, swaync-client -t"
              "$mod       , P, exec, hyprshot -m region"
              "$mod SHIFT , P, exec, hyprshot -m window"
              "$mod CTRL  , P, exec, hyprshot -m output"
              "$mod SHIFT , Q, exec, uwsm stop"
              "$mod SHIFT , S, movetoworkspace, special:magic"
              "$mod       , S, togglespecialworkspace, magic"
              "$mod       , V, togglefloating,"
              "$mod       , down, movefocus, d"
              "$mod SHIFT , down, movewindoworgroup, d"
              "$mod       , grave, layoutmsg, orientationcycle left right top center"
              "$mod       , left, movefocus, l"
              "$mod SHIFT , left, movewindoworgroup, l"
              "$mod       , mouse_down, workspace, e+1"
              "$mod       , mouse_up, workspace, e-1"
              "$mod       , return, exec, uwsm app -t service -- $terminal"
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

        systemd.user.services.kanshi.Service.ExecCondition = ''
          ${pkgs.systemd}/lib/systemd/systemd-xdg-autostart-condition "wlroots:sway:Wayfire:labwc:Hyprland" ""
        '';

        services.kanshi = {
          enable = true;
          systemdTarget = "graphical-session.target";
          settings = [
            {
              profile = {
                name = "ishamael-uw";
                outputs = [
                  {
                    criteria = out-c49rg90;
                    mode = "5120x1440";
                    status = "enable";
                  }
                  {
                    criteria = out-ishamael-edp;
                    status = "disable";
                  }
                ];
              };
            }
            {
              profile = {
                name = "ishamael-internal";
                outputs = [
                  {
                    criteria = out-ishamael-edp;
                    status = "enable";
                    mode = "3840x2400";
                  }
                ];
              };
            }
          ];
        };

        # systemd.user.services.hyprpolkitagent = {
        #   Unit = {
        #     Description = "Hyprland Polkit Authentication Agent";
        #     PartOf = "graphical-session.target";
        #     After = "graphical-session.target";
        #     ConditionEnvironment = "WAYLAND_DISPLAY";
        #   };
        #   Service = {
        #     ExecStart = "@LIBEXECDIR@/hyprpolkitagent";
        #     Slice = "session.slice";
        #     TimeoutStopSec = "5sec";
        #     Restart = "on-failure";
        #   };
        #   Install = {
        #     WantedBy = "graphical-session.target";
        #   };
        # };

        systemd.user.services.swaync = {
          Unit = {
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            Type = "simple";
            ExecStart = "${pkgs.swaynotificationcenter}/bin/swaync";
          };

          Install = {
            WantedBy = [ "hyprland-session.target" ];
          };
        };

      };
    };
}
