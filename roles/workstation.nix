{ config, pkgs, lib, bleeding, ... }:

{
  # powerManagement.enable = true;
  imports = [
    ../packages/bleeding-edge.nix
    ../packages/desktop-nagger.nix
    ../packages/haskell-packages.nix
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
    ];
    desktopPackages = with pkgs; [
      aspell
      aspellDicts.ru
      aspellDicts.en
      audacious
      chromium
      desktop-nagger
      dunst
      ebook_tools
      evince
      icewm # something to run in Xephyr
      imagemagickBig
      firefox
      geeqie
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
      stack
      stalonetray # something to make viber happy
      torbrowser
      tdesktop
      viber
      workrave
      xorg.xbacklight
      xorg.xev
      yandex-disk
    ];
    developmentPackages = with pkgs; [
      ant
      apitrace
      arduino
      autoconf
      automake
      checkbashism
      elixir
      erlangR19
      fakeroot
      gcc
      gdb
      git-review
      gnum4
      go_1_7
      libosinfo # osinfo-query
      libvirt # for `vagrant plugin install vagrant-libvirt`
      libxslt # xsltproc - for building rabbitmq
      mosh
      nodejs
      ncurses
      oraclejdk8
      pkgconfig
      python35Packages.virtualenv
      python35Packages.ipython
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
      # nix-prefetch-zip
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
      xorg = super.xorg // rec {
        xkeyboard_config_dvp = super.pkgs.lib.overrideDerivation super.xorg.xkeyboardconfig (old: {
          patches = [ ./xkb.patch ];
        });
        xorgserver = super.pkgs.lib.overrideDerivation super.xorg.xorgserver (old: {
          configureFlags = [
            "--enable-kdrive"             # not built by default
            "--enable-xephyr"
            "--enable-xcsecurity"         # enable SECURITY extension
            "--with-default-font-path="   # there were only paths containing "${prefix}",
                                          # and there are no fonts in this package anyway
            "--with-xkb-bin-directory=${xkbcomp}/bin"
            "--with-xkb-path=${xkeyboard_config_dvp}/share/X11/xkb"
            "--with-xkb-output=$out/share/X11/xkb/compiled"
            "--enable-glamor"
          ];
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
  services.dbus.packages = [ pkgs.gnome3.dconf ];
  
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
    # xkbDir ="${pkgs.xorg.xkeyboard_config_dvp}/share/X11/xkb"; # not enough - we still need to inject patched xkbcomp to xorgserver (see above)
    xkbVariant = "dvp,dvp";
    xkbOptions = "grp:menu_toggle,ctrl:nocaps,altwin:super_win,grp:sclk_toggle,ctrl:ralt_rctrl";

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
