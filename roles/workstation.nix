{ config, pkgs, lib, bleeding, ... }:

{
  imports = [
    ../packages/bleeding-edge.nix
    ../roles/emacs.nix
  ];

  nix.useChroot = true;

  boot.kernel.sysctl."vm.swappiness" = 1;

  networking.networkmanager.enable = true;
  networking.extraHosts = ''
    127.0.0.1 ${config.networking.hostName}
  '';

  # Select internationalisation properties.
  i18n = {
    consoleFont = "UniCyr_8x16";
    consoleKeyMap = "dvp";
    defaultLocale = "ru_RU.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Moscow";

  environment.systemPackages = let
    bleedingEdgePackages = with config.bleeding.pkgs; [
      arduino
      yandex-disk
      audacious
      chromium
      erlang
      firefox
      ghc
      gitAndTools.diff-so-fancy
      gitAndTools.gitFull
      platinum-searcher
      rxvt_unicode-with-plugins
      stack
      tdesktop
      tmux
      viber
      dropbox-cli
    ];
    desktopPackages = with pkgs; [
      slack
      icewm # something to run in Xephyr
      evince
      geeqie
      keepass
      libnotify
      mplayer
      playerctl
      stalonetray
      twmn
      workrave
      xorg.xbacklight
      xorg.xev
    ];
    developmentPackages = with pkgs; [
      ant
      autoconf
      automake
      checkbashism
      gcc
      git-review
      libxslt # xsltproc - for building rabbitmq
      mosh
      gdb
      gnum4
      ncurses
      oraclejdk8
      quilt
      subversion
      tightvnc
      wireshark
      wireshark-gtk
    ];
    nixDevPackages = with pkgs; [
      nix-repl
      nox
      patchelf
    ];
    utilityPackages = with pkgs; [
      sox
      telnet
      htop
      vim
      bind # for dig
      iftop
      nethogs
      lsof
      unrar
      openssl
      p7zip
      unzip
      whois
      zip
      elinks
    ];
    otherPackages = with pkgs; [
      youtube-dl
      binutils
      gnuplot
      bridge-utils
      conkeror
      cryptsetup
      gnome3.dconf
      gnome3.dconf-editor
      dmenu
      dmidecode
      dpkg
      file
      gmrun
      haskellPackages.xmobar
      haskellPackages.yeganesh
      iptables
      isyncUnstable
      gnumake
      gnupg21
      kde5.quasselClient
      keychain
      libreoffice
      manpages
      mc
      mu
      nmap
      openssl
      parallel
      pavucontrol
      pciutils
      psmisc
      python
      screen
      skype
      sudo
      tcpdump
      usbutils
      which
      wget
      wmctrl
      xclip
      xscreensaver
      zsh
    ];
  in otherPackages ++
     desktopPackages ++
     developmentPackages ++
     nixDevPackages ++
     utilityPackages ++
     bleedingEdgePackages;

  nixpkgs.config = {
    allowUnfree = true;

    firefox = {
     enableGoogleTalkPlugin = true;
     enableAdobeFlash = true;
     jre = true;
     enableDjvu = true;
    };

    chromium = {
     enablePepperFlash = true; # Chromium removed support for Mozilla (NPAPI) plugins so Adobe Flash no longer works
     enablePepperPDF = true;
    };

    packageOverrides = super: {
      # docker = pkgs.callPackage ./docker-bin.nix {};
      xorg = super.xorg // rec {
        xkeyboard_config_dvp = super.pkgs.lib.overrideDerivation super.xorg.xkeyboardconfig (old: {
          patches = [
            (builtins.toFile "ru-dvp.patch" ''
                --- xkeyboard-config-2.16-orig/symbols/ru       2014-12-11 01:56:38.000000000 +0300
                +++ xkeyboard-config-2.16/symbols/ru    2016-04-15 14:27:25.075214654 +0300
                @@ -719,3 +719,24 @@
                     key <TLDE> { [  bracketright,  bracketleft  ] };
                     key <BKSL> { [   Cyrillic_io,  Cyrillic_IO  ] };
                 };
                +partial default alphanumeric_keys
                +xkb_symbols "dvp" {
                +    include "ru(common)"
                +
                +    name[Group1]= "Russia";
                +
                +    key <AE01> {        [       numerosign, percent   ]       };
                +    key <AE02> {        [       quotedbl,   7         ]       };
                +    key <AE03> {        [       question,   5         ]       };
                +    key <AE04> {        [       slash,      3         ]       };
                +    key <AE05> {        [       parenleft,  1         ]       };
                +    key <AE06> {        [       equal,      9         ]       };
                +    key <AE07> {        [       asterisk,   0         ]       };
                +    key <AE08> {        [       parenright, 2         ]       };
                +    key <AE09> {        [       plus,       4         ]       };
                +    key <AE10> {        [       minus,      6         ]       };
                +    key <AE11> {        [       exclam,     8         ]       };
                +    key <AE12> {        [       semicolon,  colon     ]       };
                +    key <AB10> {        [       period,     comma     ]       };
                +    key <BKSL> {        [       backslash,  bar       ]       };
                +};
              '')
          ];
        });
        xorgserver = super.pkgs.lib.overrideDerivation super.xorg.xorgserver (old: {
          postInstall = ''
            rm -fr $out/share/X11/xkb/compiled
            ln -s /var/tmp $out/share/X11/xkb/compiled
            wrapProgram $out/bin/Xephyr \
              --set XKB_BINDIR "${xkbcomp}/bin" \
              --add-flags "-xkbdir ${xkeyboard_config_dvp}/share/X11/xkb"
            wrapProgram $out/bin/Xvfb \
              --set XKB_BINDIR "${xkbcomp}/bin" \
              --set XORG_DRI_DRIVER_PATH ${super.mesa}/lib/dri \
              --add-flags "-xkbdir ${xkeyboard_config_dvp}/share/X11/xkb"
            ( # assert() keeps runtime reference xorgserver-dev in xf86-video-intel and others
              cd "$dev"
              for f in include/xorg/*.h; do # */
                sed "1i#line 1 \"${old.name}/$f\"" -i "$f"
              done
            )
          '';
        });
        setxkbmap = super.pkgs.lib.overrideDerivation super.xorg.setxkbmap (old: {
          postInstall =
            ''
              mkdir -p $out/share
              ln -sfn ${xkeyboard_config_dvp}/etc/X11 $out/share/X11
            '';
        });
        xkbcomp = super.pkgs.lib.overrideDerivation super.xorg.xkbcomp (old: {
          configureFlags = "--with-xkb-config-root=${xkeyboard_config_dvp}/share/X11/xkb";
        });
      };
    };
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      google-fonts
      inconsolata
      liberation_ttf
      mplus-outline-fonts
      noto-fonts
      powerline-fonts
      terminus_font
      ubuntu_font_family
      unifont
      iosevka
      vistafonts
      source-code-pro
      fira-code
    ];
  };

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  # List services that you want to enable:
  services.gnome3.gvfs.enable = true;
  services.dbus.enable = true;
  
  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";

  services.cron.enable = true;

  services.locate.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip ];

  services.xserver = {
    config = ''
Section "InputClass"
	Identifier "CirqueTouchpad1"
	MatchProduct "GlidePoint"
	Option "SwapAxes" "True"
	Option "Emulate3Buttons" "True"
	Option "InvertY" "True"
EndSection

Section "InputClass"
	Identifier "CirqueTouchpad2"
        MatchUSBID "0488:0280"
	Option "SwapAxes" "True"
	Option "Emulate3Buttons" "True"
	Option "InvertY" "True"
EndSection

    '';
    enable = true;
    layout = "us,ru";
    xkbVariant = "dvp,dvp";
    xkbOptions = "grp:menu_toggle,ctrl:nocaps,altwin:super_win,grp:sclk_toggle";

    synaptics = {
      enable = true;
      twoFingerScroll = true;
      tapButtons = true;
      fingersMap = [0 0 0];
      buttonsMap = [0 0 0];
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    windowManager.default = "xmonad";
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";

    startOpenSSHAgent = true;

    displayManager = {
      lightdm = {
	enable = true;
      };
    };
  };

  services.redshift = {
    enable = true;
    latitude = "55.7558";
    longitude = "37.6173";
    temperature = { day = 6500; night = 3000; };
    extraOptions = [ "-v" ];
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers = {
    binarin = {
      description = "Alexey Lebedeff";
      uid = 1000;
      isNormalUser = true;
      shell = "/run/current-system/sw/bin/zsh";
      extraGroups = [ "networkmanager" "docker" "libvirtd" "wheel" "dialout" "vboxusers" ];
    };
    root = {
      shell = "/run/current-system/sw/bin/zsh";
      subUidRanges = [ { startUid = 100001; count = 65534; } ];
      subGidRanges = [ { startGid = 1001; count = 999; } ];
    };
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  networking.firewall.checkReversePath = false; # I want my DHCP for VM's

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
    # docker.extraOptions = "--insecure-registry=192.168.99.100:31500";
    # virtualbox.host.enable = true;
  };

  zramSwap = {
    enable = true;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

  programs.zsh.enable = true;
  programs.bash.enableCompletion = true;
}
