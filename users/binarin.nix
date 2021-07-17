{config, pkgs, lib, ...}:
{
  imports = [
    ../packages/user-packages.nix
  ];
  config = {
    users.extraUsers = {
      binarin = {
        description = "Alexey Lebedeff";
        uid = 1000;
        isNormalUser = true;
        shell = "/run/current-system/sw/bin/zsh";
        extraGroups = [
          "networkmanager"
          "docker"
          "libvirtd"
          "wheel"
          "dialout"
          "vboxusers"
          "wireshark"
          "transmission"
          "lxd"
          "video"
        ];
      };
    };

    programs.zsh.enable = true;
    services.autorandr.enable = true;

    home-manager.users.binarin = {
      imports = [
        ./binarin-hm.nix
      ];
      # fonts.fontconfig.enable = true;
      gtk = {
        enable = true;
        iconTheme = {
          name = "Adwaita";
          package = pkgs.gnome3.adwaita-icon-theme;
        };
        theme = {
          name = "Adwaita";
          package = pkgs.gnome3.gnome-themes-extra;
        };
      };

      programs.dircolors = {
        enable = true;
        settings = {
          DIR = "01;34;46";
        };
      };

      home.stateVersion = "20.09";
      home.sessionPath = [ "$HOME/bin" ];
      home.packages = config.userPackages;

      services.picom = {
        enable = true;
        backend = "xr_glx_hybrid";
        vSync = true;
      };

      services.dunst = {
        enable = true;
        settings = {
          global = {
            font = "sans bold 40";

            # # Allow a small subset of html markup:
            # #   <b>bold</b>
            # #   <i>italic</i>
            # #   <s>strikethrough</s>
            # #   <u>underline</u>

            # #
            # # For a complete reference see
            # # <http://developer.gnome.org/pango/stable/PangoMarkupFormat.html>.
            # # If markup is not allowed, those tags will be stripped out of the
            # # message.
            allow_markup = "yes";

            # # The format of the message.  Possible variables are:
            # #   %a  appname
            # #   %s  summary
            # #   %b  body
            # #   %i  iconname (including its path)
            # #   %I  iconname (without its path)
            # #   %p  progress value if set ([  0%] to [100%]) or nothing
            # # Markup is allowed
            format = ''<b>%s</b>\n%b'';

            # # Sort messages by urgency.
            sort = "yes";

            # # Show how many messages are currently hidden (because of geometry).
            indicate_hidden = "yes";

            # # Alignment of message text.
            # # Possible values are "left", "center" and "right".
            alignment = "left";

            # # The frequency with wich text that is longer than the notification
            # # window allows bounces back and forth.
            # # This option conflicts with "word_wrap".
            # # Set to 0 to disable.
            # bounce_freq = "0";

            # # Show age of message if message is older than show_age_threshold
            # # seconds.
            # # Set to -1 to disable.
            # show_age_threshold = "60";

            # # Split notifications into multiple lines if they don't fit into
            # # geometry.
            word_wrap = "yes";

            # # Ignore newlines '\n' in notifications.
            ignore_newline = "no";

            # # The geometry of the window:
            # #   [{width}]x{height}[+/-{x}+/-{y}]
            # # The geometry of the message window.
            # # The height is measured in number of notifications everything else
            # # in pixels.  If the width is omitted but the height is given
            # # ("-geometry x2"), the message window expands over the whole screen
            # # (dmenu-like).  If width is 0, the window expands to the longest
            # # message displayed.  A positive x is measured from the left, a
            # # negative from the right side of the screen.  Y is measured from
            # # the top and down respectevly.
            # # The width can be negative.  In this case the actual width is the
            # # screen width minus the width defined in within the geometry option.
            geometry = ''+0+35'';

            # # Shrink window if it's smaller than the width.  Will be ignored if
            # # width is 0.
            shrink = "no";

            # # The transparency of the window.  Range: [0; 100].
            # # This option will only work if a compositing windowmanager is
            # # present (e.g. xcompmgr, compiz, etc.).
            transparency = "50";

            # # Don't remove messages, if the user is idle (no mouse or keyboard input)
            # # for longer than idle_threshold seconds.
            # # Set to 0 to disable.
            idle_threshold = "120";

            # # Which monitor should the notifications be displayed on.
            monitor = "0";

            # # Display notification on focused monitor.  Possible modes are:
            # #   mouse: follow mouse pointer
            # #   keyboard: follow window with keyboard focus
            # #   none: don't follow anything
            # #
            # # "keyboard" needs a windowmanager that exports the
            # # _NET_ACTIVE_WINDOW property.
            # # This should be the case for almost all modern windowmanagers.
            # #
            # # If this option is set to mouse or keyboard, the monitor option
            # # will be ignored.
            # follow = "mouse";

            # # Should a notification popped up from history be sticky or timeout
            # # as if it would normally do.
            # sticky_history = "yes";

            # # Maximum amount of notifications kept in history
            # history_length = 20;

            # # Display indicators for URLs (U) and actions (A).
            show_indicators = "yes";

            # # The height of a single line.  If the height is smaller than the
            # # font height, it will get raised to the font height.
            # # This adds empty space above and under the text.
            # line_height = 0;

            # # Draw a line of "separatpr_height" pixel height between two
            # # notifications.
            # # Set to 0 to disable.
            separator_height = 2;

            # # Padding between text and separator.
            padding = 8;

            # # Horizontal padding.
            horizontal_padding = 8;

            # # Define a color for the separator.
            # # possible values are:
            # #  * auto: dunst tries to find a color fitting to the background;
            # #  * foreground: use the same color as the foreground;
            # #  * frame: use the same color as the frame;
            # #  * anything else will be interpreted as a X color.
            separator_color = "frame";

            # # Print a notification on startup.
            # # This is mainly for error detection, since dbus (re-)starts dunst
            # # automatically after a crash.
            # startup_notification = "false";

            # # dmenu path.
            # dmenu = "dmenu -p dunst:";

            # # Browser for opening urls in context menu.
            # browser = "xdg-open";

            # # Align icons left/right/off
            # icon_position = "off";

            # # # Paths to default icons.
            # # icon_folders = "/run/current-system/sw/share/icons/hicolor/64x64/devices:
          };

          frame = {
            width = 3;
            color = ''#aaaaaa'';
          };

          shortcuts = {
            # Close notification.
            close = "ctrl+space";

            # Close all notifications.
            close_all = "ctrl+shift+space";

            # Redisplay last message(s).
            # On the US keyboard layout "grave" is normally above TAB and left
            # of "1".
            history = "ctrl+grave";

            context = "ctrl+shift+period";
          };

          urgency_low = {
            background = ''#2eba3e'';
            foreground = ''#ffffff'';
            timeout = 10;
          };

          urgency_normal = {
            background = ''#285577'';
            foreground = ''#ffffff'';
            timeout = 10;
          };

          urgency_critical = {
            background = ''#900000'';
            foreground = ''#ffffff'';
            timeout = 0;
          };
        };
      };

      home.file.".xmonad/build" = {
        text = ''
          #!${pkgs.bash}/bin/bash
          set -euo pipefail

          nix build --no-link -f '<nixpkgs>' my-xmonad-config

          rm -f $1
          nix build -f '<nixpkgs>' -o $1 my-xmonad-executable
        '';
        force = true;
        executable = true;
      };

      xresources.properties = {
        "URxvt.termName" = "rxvt-unicode-256color";
        "URxvt.font" = "xft:Iosevka-22";
        "URxvt.boldFont" = "";
        "URxvt.italicFont" = "";
        "URxvt.boldItalicFont" = "";
        "URxvt.foreground" = "grey";
        "URxvt.background" = "black";
        "URxvt.colorUL" = "#86a2be";
        "URxvt.colorIT" = "red";
        "URxvt.underlineColor" = "orange";
        "URxvt.highlightColor" = "magenta";
        "URxvt.highlightTextColor" = "cyan";
        "URxvt.fading" = "30";
        "URxvt.fadeColor" = "black";
        "URxvt.reverseVideo" = false;
        "URxvt.pointerColor" = "red";
        "URxvt.pointerColor2" = "cyan";
        "URxvt.iso14755" = false;
        "URxvt.iso14755_52" = false;
        "URxvt.color12" = "#5555FF";
        "URxvt.urgentOnBell" = true;
        "URxvt.visualBell" = true;
        "URxvt.scrollBar" = false;
        "URxvt.perl-ext" = "filter_title";
        "URxvt.perl-ext-common" = "default,matcher,font-size,selection-to-clipboard";
        "URxvt.url-launcher" = "xdg-open";

        "URxvt.keysym.C-Up" = "font-size:increase";
        "URxvt.keysym.C-Down" = "font-size:decrease";

        "xscreensaver.newLoginCommand" = "dm-tool switch-to-greeter";
        "xscreensaver.timeout" = "5";
        "xscreensaver.lock" = true;
        "xscreensaver.lockTimeout" = "1";
        "xscreensaver.splash" = false;
        "xscreensaver.fade" = false;
        "xscreensaver.mode" = "blank";
        "xscreensaver.dpmsEnabled" = true;
        "xscreensaver.dpmsQuickOff" = true;
      };

      services.taffybar = {
        enable = true;
        package = pkgs.taffybar.override {packages = p: with p; [safe]; };
      };
      systemd.user.services.taffybar.Service.ExecStartPost = ''${pkgs.writeShellScript "kick-nm-applet" ''
        ${pkgs.coreutils}/bin/sleep 2
        ${pkgs.systemd}/bin/systemctl restart --user network-manager-applet
      ''}'';

      services.network-manager-applet.enable = true;

      xsession = {
        preferStatusNotifierItems = true;
        enable = true;
        windowManager.command = ''${pkgs.xmonad-with-packages}/bin/xmonad'';
        initExtra = ''
          autorandr -c
        '';
      };

      services.status-notifier-watcher.enable = true;

      programs.autorandr = {
        enable = true;
        hooks = {
          postswitch = {
            taffy = ''${pkgs.systemd}/bin/systemctl restart --user taffybar'';
            bh = ''${pkgs.feh}/bin/feh --bg-scale ${./green_3-wallpaper-5120x1440.jpg}'';
          };
        };
        profiles = let
          cf-home = {
            HDMI-A-1 = {
              enable = true;
              crtc = 3;
              mode = "1920x1080";
              position = "640x0";
              rate = "60.00";
            };
            DisplayPort-0 = {
              enable = true;
              crtc = 2;
              mode = "1920x1080";
              position = "2560x0";
              rate = "60.00";
            };
            DisplayPort-1 = {
              enable = true;
              primary = true;
              crtc = 0;
              mode = "5120x1440";
              position = "0x1080";
              rate = "59.98";
            };
          };
        in {
          home-split = {
            fingerprint = {
              DisplayPort-0 = "00ffffffffffff0038a316680000000028150104a5331d78e25ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0031583331363230374e420a20200114020313c1469004031f13122309070783010000011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018023a80d072382d40102c4580fe1f1100001e0000000000000000000000000000000000000000000000000000000000000000000000004d";
              DisplayPort-1 = "00ffffffffffff004c2d9c0f3056574331390104b57722783aa2a1ad4f46a7240e5054bfef80714f810081c08180a9c0b3009500d1c05aa000a0a0a0465030203500a9504100001a000000fd0032641ea02a000a202020202020000000fc00433439524739780a2020202020000000ff0048345a4d4330303437330a20200182020311f044105a405b2309070783010000565e00a0a0a0295030203500a9504100001a584d00b8a1381440f82c4500a9504100001e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c2";
              HDMI-A-0 = "00ffffffffffff0038a31568010101012c14010380331d78ea5ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0030593130313539334e420a2020012c02010400011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018011d00bc52d01e20b8285540fe1f1100001e023a80d072382d40102c4580fe1f1100001e00000000000000000000000000000000000000000000000000000000000000000003";
            };
            config = {
              HDMI-A-0 = {
                enable = true;
                crtc = 3;
                mode = "1920x1080";
                position = "0x0";
                rate = "60.00";
              };
              DisplayPort-0 = {
                enable = true;
                crtc = 2;
                mode = "1920x1080";
                position = "1920x0";
                rate = "60.00";
              };
              DisplayPort-1 = {
                enable = true;
                primary = true;
                crtc = 0;
                mode = "2560x1440";
                position = "0x1080";
                rate = "59.95";
              };
            };
          };
          home-gaming = {
            fingerprint = {
              DisplayPort-0 = "00ffffffffffff0038a316680000000028150104a5331d78e25ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0031583331363230374e420a20200114020313c1469004031f13122309070783010000011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018023a80d072382d40102c4580fe1f1100001e0000000000000000000000000000000000000000000000000000000000000000000000004d";
              DisplayPort-1 = "00ffffffffffff004c2d9c0f3056574331390104b57722783ba2a1ad4f46a7240e5054bfef80714f810081c08180a9c0b3009500d1c074d600a0f038404030203a00a9504100001a000000fd003078bebe61010a202020202020000000fc00433439524739780a2020202020000000ff0048345a4d4330303437330a2020025102032cf046105a405b3f5c2309070783010000e305c0006d1a0000020f307800048b127317e60605018b7312565e00a0a0a0295030203500a9504100001a584d00b8a1381440f82c4500a9504100001e1a6800a0f0381f4030203a00a9504100001af4b000a0f038354030203a00a9504100001a0000000000000000000000af701279000003013c57790188ff139f002f801f009f055400020009006c370108ff139f002f801f009f0545000200090033b70008ff139f002f801f009f0528000200090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f390";
              HDMI-A-1 = "00ffffffffffff0038a31568010101012c14010380331d78ea5ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0030593130313539334e420a2020012c02010400011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018011d00bc52d01e20b8285540fe1f1100001e023a80d072382d40102c4580fe1f1100001e00000000000000000000000000000000000000000000000000000000000000000003";
            };
            config = {
              HDMI-A-1 = {
                enable = false;
                crtc = 1;
                mode = "1920x1080";
                position = "0x0";
                rate = "60.00";
                primary = true;
              };
              DisplayPort-0 = {
                enable = true;
                crtc = 0;
                mode = "1920x1080";
                position = "1920x0";
                rate = "60.00";
              };
              DisplayPort-1 = {
                enable = false;
              };
            };
          };
          single = {
            fingerprint = {
              DisplayPort-1 = "00ffffffffffff004c2d9c0f3056574331390104b57722783aa2a1ad4f46a7240e5054bfef80714f810081c08180a9c0b3009500d1c01a6800a0f0381f4030203a00a9504100001a000000fd00324b1e5a2f000a202020202020000000fc00433439524739780a2020202020000000ff0048345a4d4330303437330a2020029d02031af042105a2309070783010000e305c000e60605018b7312584d00b8a1381440f82c4500a9504100001e565e00a0a0a0295030203500a9504100001a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ac701279000003011433b70088ff139f002f801f009f0528000200090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009590";
              HDMI-A-0 = "00ffffffffffff0038a31568010101012c14010380331d78ea5ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0030593130313539334e420a2020012c02010400011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018011d00bc52d01e20b8285540fe1f1100001e023a80d072382d40102c4580fe1f1100001e00000000000000000000000000000000000000000000000000000000000000000003";
            };
            config = {
              HDMI-A-0 = {
                enable = false;
              };
              DisplayPort-0 = {
                enable = false;
              };
              DisplayPort-1 = {
                enable = true;
                primary = true;
                crtc = 0;
                mode = "5120x1440";
                position = "0x1080";
                rate = "59.98";
              };
            };
          };
          home = {
            config = cf-home;
            fingerprint = {
              DisplayPort-0 = "00ffffffffffff0038a316680000000028150104a5331d78e25ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0031583331363230374e420a20200114020313c1469004031f13122309070783010000011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018023a80d072382d40102c4580fe1f1100001e0000000000000000000000000000000000000000000000000000000000000000000000004d";
              DisplayPort-1 = "00ffffffffffff004c2d9c0f000000002b1c0104b57722783aa2a1ad4f46a7240e5054bfef80714f810081c08180a9c0b3009500d1c01a6800a0f0381f4030203a00a9504100001a000000fd00324b1e5a2f000a202020202020000000fc00433439524739780a2020202020000000ff004831414b3530303030300a2020021a02031af042105a2309070783010000e305c000e60605018b7312584d00b8a1381440f82c4500a9504100001e565e00a0a0a0295030203500a9504100001a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ac701279000003011433b70088ff139f002f801f009f0528000200090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009590";
              HDMI-A-1 = "00ffffffffffff0038a31568010101012c14010380331d78ea5ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0030593130313539334e420a2020012c02010400011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018011d00bc52d01e20b8285540fe1f1100001e023a80d072382d40102c4580fe1f1100001e00000000000000000000000000000000000000000000000000000000000000000003";
            };
          };
          home-1 = {
            config = cf-home;
            fingerprint = {
              DisplayPort-0 = "00ffffffffffff0038a316680000000028150104a5331d78e25ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0031583331363230374e420a20200114020313c1469004031f13122309070783010000011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018023a80d072382d40102c4580fe1f1100001e0000000000000000000000000000000000000000000000000000000000000000000000004d";
              DisplayPort-1 = "00ffffffffffff004c2d9c0f000000002b1c0104b57722783aa2a1ad4f46a7240e5054bfef80714f810081c08180a9c0b3009500d1c01a6800a0f0381f4030203a00a9504100001a000000fd00324b1e5a2f000a202020202020000000fc00433439524739780a2020202020000000ff004831414b3530303030300a2020021a02031af042105a2309070783010000e305c000e60605018b7312584d00b8a1381440f82c4500a9504100001e565e00a0a0a0295030203500a9504100001a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ac701279000003011433b70088ff139f002f801f009f0528000200090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009590";
              HDMI-A-1 = "00ffffffffffff0038a31568010101012c14010380331d78ea5ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0030593130313539334e420a2020012c02010400011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018011d00bc52d01e20b8285540fe1f1100001e023a80d072382d40102c4580fe1f1100001e00000000000000000000000000000000000000000000000000000000000000000003";
            };
          };
        };
      };
    };
    nix.trustedUsers = [ "binarin" ];
  };
}
