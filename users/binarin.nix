{config, pkgs, lib, ...}:
{
  imports = [
    ../packages/user-packages.nix
  ];

  config = {
    nix.trustedUsers = [ "binarin" ];

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
      ] ++ lib.optional (pkgs.system == "x86_64-linux") ./binarin-hm-linux.nix;

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

      home.stateVersion = "22.05";
      home.sessionPath = [ "$HOME/bin" ];
      home.packages = config.userPackages;

      # services.picom = {
      #   enable = true;
      #   backend = "glx";
      #   vSync = true;
      #   fade = false;
      #   shadow = false;
      #   blur = false;
      # };

      services.dunst = {
        enable = false;
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

      home.file.".urxvt/ext/filter_title_and_paste" = {
        text = ''
          #! perl
          use 5.14.0;
          use Data::Dumper;
          use autodie;

          # Prohibit changing terminal title
          sub on_osc_seq {
              my ($term, $op, $args, $resp) = @_;
              if( $op == 0 ) {
                  return 1;
              }
              return;
          }

          # Strip end-paste ESC-sequence
          sub on_tt_paste {
              my ($term, $octets) = @_;
              $octets =~ s/\[201~/^[[201~/g;
              $term->tt_paste($octets);
              return 1;
          }
        '';
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
        "URxvt.perl-ext" = "filter_title_and_paste";
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

      services.network-manager-applet.enable = true;
    };
  };
}
