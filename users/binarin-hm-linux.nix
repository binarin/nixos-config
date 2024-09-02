{lib, pkgs, config, system, ...}:

let
  texlive-combined = pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-full beamer ps2eps;
  };
  out-u4025qw = "Dell Inc. DELL U4025QW J7Q6FP3";
  out-lg-dualup-left = "LG Electronics LG SDQHD 311NTQDAC572";
  out-lg-dualup-right = "LG Electronics LG SDQHD 311NTSUAC574";
  out-ishamael-edp = "Sharp Corporation 0x1516 Unknown";
  out-c49rg90 = "Samsung Electric Company C49RG9x H1AK500000";
in {
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "x-scheme-handler/https" = "smart-browser-chooser.desktop";
      "x-scheme-handler/http" = "smart-browser-chooser.desktop";
      "x-scheme-handler/org-protocol" = "org-protocol.desktop";
      "x-scheme-handler/tg" = "org.telegram.desktop.desktop";
      "image/jpeg" = "geeqie.desktop";
    };
    associations.added = {
      "application/pdf" = "org.gnome.Evince.desktop";
      "image/jpeg" = "geeqie.desktop";
    };
  };

  home.sessionVariables.MOZ_ENABLE_WAYLAND = "1";
  home.sessionVariables.NIXOS_OZONE_WL = "1";
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
    swaylock
    swayidle
    wl-clipboard
    mako # notification daemon
    alacritty # Alacritty is the default terminal in the config
    dmenu # Dmenu is the default in the config but i recommend wofi since its wayland native
    swaykbdd
    fuzzel
    swaynotificationcenter
    (pkgs.writeShellScriptBin "x-www-browser" ''
      exec firefox "$@"
    '')
  ];

  xdg.configFile."mimeapps.list".force = true;
  home.activation = {
    removeCommonConfictingFiles = lib.hm.dag.entryBefore ["checkLinkTargets"] ''
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
        "protonmail-bridge -n"
        "[workspace 1 silent] foot tmux new-session -A -s binarin"
        "[workspace 2 silent] emacs"
        "[workspace 4 silent] firefox"
        "[workspace 5 silent; togglegroup] thunderbird"
        "[workspace 5 silent] telegram-desktop"
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
        "$mod       , N, exec, swaync-client -t"
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
      ];
      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];
    };
  };

  wayland.windowManager.sway = {
    enable = true;
    package = pkgs.sway;
    wrapperFeatures.gtk = true ;
    extraSessionCommands = ''
      source ~/.nix-profile/etc/profile.d/hm-session-vars.sh
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
    extraConfig = ''
      exec "pkill -f kanshi"
      exec "xrdb -merge ~/.Xresources"
    '';

    config = {
      bars = [ ];
      terminal = "urxvt";
      # menu = "yeganesh -x | ${pkgs.findutils}/bin/xargs swaymsg exec --";
      menu = "fuzzel";
      modifier = "Mod4";

      input = {
        # TODO Calibration matrix only works for absolute positioning devices
        # "1160:640:Cirque_Corporation_9925_AG_Touchpad" = {
        #   calibration_matrix = "0 1 0 -1 0 1";
        # };
        "*" = {
          xkb_layout = "us,ru";
          xkb_variant = ",winkeys";
          xkb_options = "grp:menu_toggle,ctrl:nocaps,altwin:super_win,grp:sclk_toggle,compose:paus";
        };
      };

      window = {
        border = 3;
        commands = [
          {
            command = "border pixel 0";
            criteria = { title = "SWAY_SPACER"; };
          }
          {
            command = "floating enable";
            criteria = { app_id = "pavucontrol"; };
          }

          {
            criteria = { workspace = "9"; };
            command = "floating enable";
          }
        ];
      };

      bindkeysToCode = true;
      keybindings = lib.mkOptionDefault {
          "Mod4+Ctrl+h" = "resize shrink width 10";
          "Mod4+Ctrl+j" = "resize shrink height 10";
          "Mod4+Ctrl+k" = "resize grow height 10";
          "Mod4+Ctrl+l" = "resize grow width 10";
          "Mod4+Shift+Return" = "exec urxvt";
          "Mod4+Return" = "exec ~/bin/sway-clear";
          "Mod4+semicolon" = "exec ${./sshmenu}";
          "Mod4+n" = "exec swaync-client -t";
          "Mod4+x" = "[urgent=latest] focus";
          "Mod4+Shift+c" = "kill";
          "Mod4+g" = "gaps horizontal current plus 500";
          "Mod4+Shift+g" = "gaps horizontal current minus 500";
          "Ctrl+Backslash" = "exec ${./sway-layout-switch.sh} doit";
          "Mod4+i" = "exec ${./passmenu}";
          "Mod4+Shift+i" = "exec ${./passmenu} --type";
      };
    };
  };

  home.file."bin/sway-clear".source = ./sway-clear.py;

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

  services.swayidle = {
    enable = true;
    timeouts = [
      { timeout = 300; command = ''${pkgs.hyprland}/bin/hyprctl dispatch dpms off''; resumeCommand = ''${pkgs.hyprland}/bin/hyprctl dispatch dpms on''; }
      { timeout = 360; command = ''${pkgs.swaylock}/bin/swaylock -f -c 000000''; }
    ];
    events = [
      { event = "before-sleep"; command = "${pkgs.swaylock}/bin/swaylock -f -c 000000"; }
    ];
  };

  systemd.user.services.swaykbdd = {
    Unit = {
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      Type = "simple";
      ExecStart =
        "${pkgs.swaykbdd}/bin/swaykbdd";
    };

    Install = { WantedBy = [ "sway-session.target" ]; };
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

  systemd.user.services.sway-layout-switch = {

    Unit = {
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      Type = "simple";
      ExecStart = "${./sway-layout-switch.sh}";
      Environment = [ "PATH=${lib.makeBinPath (with pkgs; [ bash sway jq ])}" ];
    };

    Install = { WantedBy = [ "sway-session.target" ]; };
  };

  home.file.".config/swaync/config.json".text = ''
    {
      "scripts": {
      }
    }
  '';
  programs.home-manager.enable = true;
  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "Iosevka:size=20, Noto Color Emoji:size=16";
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

        "hyprland/workspaces" = {
        };

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
    style = ./waybar-style.css;
  };

  home.file.".config/waybar/base16-zenburn.css".source = ./base16-zenburn.css;

  xdg.dataFile."dbus-1/services/org.freedesktop.secrets.service".text = ''
    [D-BUS Service]
    Name=org.freedesktop.secrets
    Exec=${lib.getBin pkgs.plasma5Packages.kwallet}/bin/kwalletd5
  '';

}
