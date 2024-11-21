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
      sshmenu
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
        # {
        #   profile = {
        #     name = "valak";
        #     outputs = [
        #       {
        #         criteria = out-lg-dualup-left;
        #         status = "enable";
        #         mode = "2560x2880@60Hz";
        #         position = "0,0";
        #       }
        #       {
        #         criteria = out-u4025qw;
        #         status = "enable";
        #         mode = "5120x2160@120Hz";
        #         position = "2560,332";
        #       }
        #       {
        #         criteria = out-lg-dualup-right;
        #         status = "enable";
        #         mode = "2560x2880@60Hz";
        #         position = "7680,0";
        #       }
        #     ];
        #   };
        # }
        {
          profile = {
            name = "valak-hidpi";
            outputs = [
              {
                criteria = out-lg-dualup-left;
                status = "enable";
                mode = "2560x2880@60Hz";
                position = "0,0";
                scale = 2.0;
              }
              {
                criteria = out-u4025qw;
                status = "enable";
                mode = "5120x2160@120Hz";
                position = "1280,165";
                scale = 2.0;
              }
              {
                criteria = out-lg-dualup-right;
                status = "enable";
                mode = "2560x2880@60Hz";
                position = "3840,0";
                scale = 2.0;
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

    xdg.dataFile."dbus-1/services/org.freedesktop.secrets.service".text = ''
      [D-BUS Service]
      Name=org.freedesktop.secrets
      Exec=${lib.getBin pkgs.plasma5Packages.kwallet}/bin/kwalletd5
    '';

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
            on-resume  = "brightnessctl-all -r"; # monitor backlight restore.
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
