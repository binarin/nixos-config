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
    };
    associations.added = {
      "application/pdf" = "org.gnome.Evince.desktop";
    };
  };

  # xsession.importedVariables = [
  #   "WAYLAND_DISPLAY"
  #   "SWAYSOCK"
  # ];

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
  ];

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true ;
    extraSessionCommands = ''
      source ~/.nix-profile/etc/profile.d/hm-session-vars.sh
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
    extraConfig = ''
      exec "pkill -f kanshi"
    '';

    config = {
      terminal = "urxvt";
      menu = "yeganesh -x | ${pkgs.findutils}/bin/xargs swaymsg exec --";
      modifier = "Mod4";

      input = {
        # TODO Calibration matrix only works for absolute positioning devices
        # "1160:640:Cirque_Corporation_9925_AG_Touchpad" = {
        #   calibration_matrix = "0 1 0 -1 0 1";
        # };
        "*" = {
          xkb_layout = "us,ru";
          xkb_variant = ",winkeys";
          xkb_options = "grp:menu_toggle,ctrl:nocaps,altwin:super_win,grp:sclk_toggle";
        };
      };

      window = {
        border = 3;
        commands = [
          {
            command = "border pixel 0";
            criteria = { title = "SWAY_SPACER"; };
          }
        ];
      };

      keybindings = lib.mkOptionDefault {
          "Mod4+Ctrl+h" = "resize shrink width 10";
          "Mod4+Ctrl+j" = "resize shrink height 10";
          "Mod4+Ctrl+k" = "resize grow height 10";
          "Mod4+Ctrl+l" = "resize grow width 10";
          "Mod4+Shift+Return" = "exec urxvt";
          "Mod4+Return" = "exec sway-clear";
          "Mod4+semicolon" = "exec sshmenu";
          "Mod4+x" = "[urgent=latest] focus";
          "Mod4+Shift+c" = "kill";
          "Mod4+g" = "gaps horizontal current plus 500";
          "Mod4+Shift+g" = "gaps horizontal current minus 500";
          "Ctrl+Backslash" = "exec ${./sway-layout-switch.sh} doit";
      };
    };
  };

  home.file."bin/sway-clear".source = ./sway-clear.py;

  services.kanshi = {
    enable = true;
    profiles = {
      uw = {
        outputs = [
          {
            criteria = "Samsung Electric Company C49RG9x H4ZMC00473";
            mode = "5120x1440";
            status = "enable";
          }
        ];
      };
    };
  };

  services.swayidle = {
    enable = true;
    timeouts = [
      { timeout = 300; command = ''swaymsg "output * dpms off"''; resumeCommand = ''swaymsg "output * dpms on"''; }
      { timeout = 360; command = "swaylock -f -c 000000"; }
    ];
    events = [
      { event = "before-sleep"; command = "swaylock -f -c 000000"; }
    ];
  };

  systemd.user.services.swaykbdd = {
    Unit = {
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      Type = "simple";
      ExecStart =
        "${pkgs.bleeding.swaykbdd}/bin/swaykbdd";
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
    };

    Install = { WantedBy = [ "sway-session.target" ]; };
  };
}
