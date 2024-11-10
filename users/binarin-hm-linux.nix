{ lib, pkgs, config, system, ... }:

let
  texlive-combined = pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-full beamer ps2eps;
  };
  out-u4025qw = "Dell Inc. DELL U4025QW J7Q6FP3";
  out-lg-dualup-left = "LG Electronics LG SDQHD 311NTQDAC572";
  out-lg-dualup-right = "LG Electronics LG SDQHD 311NTSUAC574";
  out-ishamael-edp = "Sharp Corporation 0x1516 Unknown";
  out-c49rg90 = "Samsung Electric Company C49RG9x H1AK500000";
in
{
  config = lib.mkIf pkgs.stdenv.isLinux {
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/org-protocol" = "org-protocol.desktop";
        "x-scheme-handler/tg" = "org.telegram.desktop.desktop";
        "image/jpeg" = "geeqie.desktop";
      };
      associations.added = {
        "application/pdf" = "org.gnome.Evince.desktop";
        "image/jpeg" = "geeqie.desktop";
      };
    };

    home.sessionVariables = {
      MOZ_ENABLE_WAYLAND = "1";
      NIXOS_OZONE_WL = "1";
      SDL_VIDEODRIVER = "wayland";

      # needs qt5.qtwayland in systemPackages
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";

      # Fix for some Java AWT applications (e.g. Android Studio),
      # use this if they aren't displayed properly:
      "_JAVA_AWT_WM_NONREPARENTING" = "1";
    };

    xsession.preferStatusNotifierItems = true;

    services.gpg-agent = {
      enable = true;
      defaultCacheTtl = 3600;
      maxCacheTtl = 14400;
      extraConfig = ''
        allow-preset-passphrase
      '';
      #pinentryFlavor = "gtk2";
      pinentryPackage = pkgs.pinentry-gtk2;
    };

    home.file."bin/ps2eps" = {
      source = "${texlive-combined}/share/texmf/scripts/ps2eps/ps2eps.pl";
    };

    home.file.".config/libvirt/libvirt.conf".text = ''
      uri_default = "qemu:///system"
    '';


    home.file.".config/looking-glass/client.ini".text = ''
      [app]
      shmFile=/dev/shm/looking-glass
      renderer=auto
      allowDMA=yes

      [win]
      title=looking-glass-client
      autoResize=yes
      keepAspect=yes
      dontUpscale=yes
      noScreensaver=yes
      quickSplash=yes
      borderless=no
      fullScreen=yes
      uiFont=pango:Iosevka
      uiSize=16
      maximize=no
      showFPS=no

      [egl]
      vsync=yes
      multisample=yes
      scale=2

      [wayland]
      warpSupport=yes
      fractionScale=no

      # [input]
      # escapeKey=70
      # grabKeyboardOnFocus=yes
      # releaseKeysOnFocusLoss=yes
      # autoCapture=yes
      # rawMouse=yes

      # [spice]
      # enable=yes
      # host=/dev/shm/win10-4game_spice
      # port=5900
      # clipboard=yes
      # clipboardToVM=yes
      # clipboardToLocal=yes
    '';

    home.packages = with pkgs; [
      # ryujinx
      (bleeding.flameshot.override { enableWlrSupport = true; })
      bibata-cursors
      steam-run
      grimblast
      trezor-agent
      kanshi
      distrobox
      hunspellDicts.nl_nl
      ddcutil
      ydotool
      dmenu-wayland
      arduino
      texlive-combined
      abcm2ps
      wl-clipboard
      dmenu # Dmenu is the default in the config but i recommend wofi since its wayland native
      fuzzel
      swaynotificationcenter
      xorg.xhost
      (pkgs.writeShellScriptBin "x-www-browser" ''
        exec firefox "$@"
      '')
    ];

    xdg.configFile."mimeapps.list".force = true;
    home.activation = {
      removeCommonConfictingFiles = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
        $DRY_RUN_CMD rm -fv ~/.gtkrc-2.0 ~/.gtkrc-2.0.backup
      '';
    };

    wayland.windowManager.hyprland = {
      enable = true;
      # package = pkgs.hyprland;
      settings = {
        misc = {
          force_default_wallpaper = 0;
          disable_hyprland_logo = true;
          background_color = "0x00807F";
        };

        "$terminal" = "foot";
        "$menu" = "fuzzel";
        "$fileManager" = "dolphin";
        "$mod" = "SUPER";
        "$hyper" = "SUPER SHIFT ALT CTRL";
        "$col_active" = "0xffaaffaa";
        "$col_inactive" = "0xff999999";

        exec-once = [
          "${pkgs.kwallet-pam}/libexec/pam_kwallet_init --no-startup-id"
          "hyprctl setcursor Bibata-Modern-Amber 48"
          "protonmail-bridge -n"
          "nm-applet"
          "[workspace 1 silent] foot --title 'SH|LOCAL' -e tmux new-session -A -s binarin"
          "[workspace 2 silent] emacs"
          "[workspace 4 silent] firefox"
          "[workspace 5 silent] sleep 5; exec thunderbird" # give protonmail-bridge time to startup
          "[workspace 5 silent; group new] telegram-desktop"
          "waybar"
        ];

        general = {
          border_size = 3;
          "col.active_border" = "$col_active";
          "col.inactive_border" = "$col_inactive";
          resize_on_border = true;
          layout = "master";
        };

        group = {
          "col.border_active" = "$col_active";
          "col.border_inactive" = "$col_inactive";
          groupbar = {
            "col.active" = "0xff66bb66";
            "col.inactive" = "$col_inactive";
          };
        };

        animation = [
          "workspaces,0"
        ];

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

    services.kanshi = {
      enable = true;
      # package = pkgs.kanshi; # at least 1.3.1
      systemdTarget = "hyprland-session.target";
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
        {
          profile = {
            name = "valak";
            outputs = [
              {
                criteria = out-lg-dualup-left;
                status = "enable";
                mode = "2560x2880@60Hz";
                position = "0,0";
              }
              {
                criteria = out-u4025qw;
                status = "enable";
                mode = "5120x2160@120Hz";
                position = "2560,332";
              }
              {
                criteria = out-lg-dualup-right;
                status = "enable";
                mode = "2560x2880@60Hz";
                position = "7680,0";
              }
            ];
          };
        }
      ];
    };

    systemd.user.services.swaync = {
      Unit = {
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        Type = "simple";
        ExecStart =
          "${pkgs.swaynotificationcenter}/bin/swaync";
      };

      Install = { WantedBy = [ "hyprland-session.target" ]; };
    };

    home.file.".config/swaync/config.json".text = ''
      {
        "scripts": {
        }
      }
    '';

    programs.home-manager.enable = true;
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
      style = pkgs.writeText "waybar-style.css" (builtins.readFile ./waybar-style.css);
    };

    home.file.".config/waybar/base16-zenburn.css".source = pkgs.writeText "base16-zenburn.css" (builtins.readFile ./base16-zenburn.css);

    xdg.dataFile."dbus-1/services/org.freedesktop.secrets.service".text = ''
      [D-BUS Service]
      Name=org.freedesktop.secrets
      Exec=${lib.getBin pkgs.plasma5Packages.kwallet}/bin/kwalletd5
    '';

    home.pointerCursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Amber";
      size = 48;
      x11.enable = true;
      gtk.enable = true;
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

        input-field = [
          {
            size = "200, 50";
            position = "0, -80";
            monitor = "";
            dots_center = true;
            fade_on_empty = false;
            font_color = "rgb(202, 211, 245)";
            inner_color = "rgb(91, 96, 120)";
            outer_color = "rgb(24, 25, 38)";
            outline_thickness = 5;
            placeholder_text = ''<span foreground="##cad3f5">Password...</span>'';
            shadow_passes = 2;
          }
        ];
      };
    };

    services.hypridle = {
      enable = true;
      settings = {
        general = {
          lock_cmd = "pidof hyprlock || hyprlock"; # avoid starting multiple hyprlock instances.
          before_sleep_cmd = "loginctl lock-session"; # lock before suspend.
          after_sleep_cmd = "hyprctl dispatch dpms on"; # to avoid having to press a key twice to turn on the display.
        };
        listener = [

          {
            timeout = 150; # 2.5min.
            on-timeout = "brightnessctl-all -s set 10"; # set monitor backlight to minimum, avoid 0 on OLED monitor.
            on-resume = "brightnessctl-all -r"; # monitor backlight restore.
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
  };
}
