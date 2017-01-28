{ config, pkgs, lib, bleeding, ... }:

{
  # powerManagement.enable = true;
  imports = [
    ../packages/bleeding-edge.nix
    ../roles/emacs.nix
  ];

  # services.teamviewer.enable = true;

  nix.useSandbox = true;
  nix.extraOptions = ''
    gc-keep-outputs = true
    gc-keep-derivations = true
  '';

  boot.kernel.sysctl."vm.swappiness" = 1;

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelModules = [ "fuse" ];

  networking.networkmanager.enable = true;

  networking.extraHosts = ''
    127.0.0.1 ${config.networking.hostName}
  '';

  networking.nat.enable = true;
  networking.nat.internalInterfaces = ["lxc0"];
  networking.nat.externalInterface = "enp3s0";

  networking.bridges = {
    lxc0 = {
      interfaces = [];
    };
  };

  networking.interfaces = {
    lxc0 = {
      ipAddress = "10.10.30.1";
      prefixLength = 24;
    };
  };

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
      firefox
      stack
      haskellPackages.threadscope
      viber
      yandex-disk
    ];
    desktopPackages = with pkgs; [
      audacious
      chromium
      dunst
      ebook_tools
      evince
      icewm # something to run in Xephyr
      geeqie
      ghc
      gitAndTools.diff-so-fancy
      gitAndTools.gitFull
      gitg
      glxinfo
      keepass
      libnotify
      mplayer
      playerctl
      rxvt_unicode-with-plugins
      shutter
      slack
      stalonetray # something to make viber happy
      tdesktop
      workrave
      xorg.xbacklight
      xorg.xev
    ];
    developmentPackages = with pkgs; [
      ant
      apitrace
      arduino
      autoconf
      automake
      checkbashism
      elixir
      erlang
      fakeroot
      gcc
      gdb
      git-review
      gnum4
      libvirt # for `vagrant plugin install vagrant-libvirt`
      libxslt # xsltproc - for building rabbitmq
      mosh
      nodejs
      ncurses
      oraclejdk8
      pkgconfig
      python35Packages.virtualenv
      quilt
      sbcl
      subversion
      tightvnc
      travis
      wireshark
      wireshark-gtk
    ];
    nixDevPackages = with pkgs; [
      nix-repl
      nix-prefetch-zip
      nixops
      nox
      patchelf
    ];
    utilityPackages = with pkgs; [
      apg
      bind # for dig
      dropbox-cli
      elinks
      fzf
      jekyll
      htop
      httpie
      iftop
      lsof
      nethogs
      nfs-utils # for vagrant
      openssl
      p7zip
      platinum-searcher
      rtorrent
      sox
      syncthing
      telnet
      texlive.combined.scheme-full
      tmux
      unrar
      unzip
      vagrant
      vim
      virt-viewer
      virtmanager
      whois
      zip
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
      gnupg1orig # some scripts expect this version
      gnupg21
      quasselClient
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
      rpm
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
      # mesa_noglu = super.mesa_noglu.override {
      #   enableTextureFloats = true;
      # };
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
      font-awesome-ttf
      corefonts
      config.bleeding.pkgs.google-fonts
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

  hardware.opengl = let
    custom-mesa = pkgs.mesa_noglu.override {
      enableTextureFloats = true;
    };
  in {
    enable = true;
    package = pkgs.buildEnv {
      name = "opengl-hack";
      # NOTE: Forces open source S2TC rather than S3TC
      paths = [ custom-mesa custom-mesa.drivers pkgs.libtxc_dxtn_s2tc ];
    };
  };

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  # List services that you want to enable:
  services.gnome3.gvfs.enable = true;
  services.gnome3.at-spi2-core.enable = true; # https://github.com/NixOS/nixpkgs/issues/16327
  services.dbus.enable = true;
  
  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";
  services.openssh.forwardX11 = true;

  services.cron.enable = true;
  # services.dnsmasq.enable = true;
  services.locate.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip ];

  services.xserver = {
    # videoDrivers = [ "ati_unfree" "ati" "noveau" "intel" "vesa" "vmware" "modesetting"];
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

  networking.firewall.enable = true;
  networking.firewall.checkReversePath = false; # I want my DHCP for VM's

  virtualisation = {
    lxc.enable = true;
    lxc.defaultConfig = ''
      lxc.network.type = veth
      lcx.network.link = lxc0
    '';

    docker.enable = true;
    docker.storageDriver = "devicemapper";

    libvirtd.enable = true;
  };

  zramSwap = {
    enable = true;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

  programs.ssh.startAgent = true;
  programs.zsh.enable = true;
  programs.bash.enableCompletion = true;

  # ghcjs
  nix.trustedBinaryCaches = [ "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

  systemd.services."binarin-org-sync" = let
    script = pkgs.writeScript "binarin-org-sync" ''
      #!${pkgs.bash}/bin/bash
      set -euo pipefail
      export NIX_REMOTE=daemon
      export NIX_PATH=nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos/nixpkgs:nixos-config=/etc/nixos/configuration.nix:/nix/var/nix/profiles/per-user/root/channels
      if [[ -f /home/binarin/org/push.sh ]]; then
        exec /home/binarin/org/push.sh
      fi
    '';
  in {
    description = "Syncs org-mode files via git";
    path = [ pkgs.gitAndTools.gitFull pkgs.wget pkgs.nix pkgs.bash pkgs.nettools ];
    serviceConfig = {
      Type = "oneshot";
      User = "binarin";
      ExecStart = script;
    };
  };
  systemd.timers."binarin-org-sync" = {
    description = "Periodically updates org-mode files via git";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "1min";
      OnUnitActiveSec = "15min";
    };
  };

}
