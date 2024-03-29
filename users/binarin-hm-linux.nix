{lib, pkgs, config, system, ...}:

let texlive-combined = pkgs.texlive.combine {
  inherit (pkgs.texlive) scheme-full beamer ps2eps;
};
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
    pinentryFlavor = "gtk2";
  };

  home.file."bin/ps2eps" = {
    source = "${texlive-combined}/share/texmf/scripts/ps2eps/ps2eps.pl";
  };

  home.packages = with pkgs; [
    bleeding.grapejuice
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
    package = pkgs.kanshi; # at least 1.3.1
    profiles = let
      out-ishamael-edp = "Sharp Corporation 0x1516 Unknown";
      out-c49rg90 = "Samsung Electric Company C49RG9x H1AK500000";
    in {
      ishamael-uw = {
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
      ishamael-internal = {
        outputs = [
          {
            criteria = out-ishamael-edp;
            status = "enable";
            mode = "3840x2400";
          }
        ];
      };
    };
  };

  services.swayidle = {
    enable = true;
    timeouts = [
      { timeout = 300; command = ''${pkgs.sway}/bin/swaymsg "output * dpms off"''; resumeCommand = ''${pkgs.sway}/bin/swaymsg "output * dpms on"''; }
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

    Install = { WantedBy = [ "sway-session.target" ]; };
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
    systemd.enable = true;
    settings = {
      mainBar = {
        position = "bottom";
        layer = "top";
        height = 34;
        # output = [
        #   "eDP-1"
        #   "HDMI-A-1"
        # ];

        modules-left = [ "sway/workspaces" "sway/mode" "wlr/taskbar" ];
        modules-center = [ "sway/window" ];
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
          exec-if = "which swaync-client";
          exec = "swaync-client -swb";
          on-click = "swaync-client -t -sw";
          on-click-right = "swaync-client -d -sw";
          escape = true;
        };

        "tray" = {
          icon-size = 32;
        };

        "wlr/taskbar" = {
          on-click = "activate";
        };

        "sway/workspaces" = {
          disable-scroll = true;
          all-outputs = true;
          persistent_workspaces = {
            "1" = [];
            "2" = [];
            "3" = [];
            "4" = [];
            "5" = [];
            "6" = [];
            "7" = [];
            "8" = [];
            "9" = [];
          };
        };

        "sway/mode" = {
          format = " {} ";
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
}
