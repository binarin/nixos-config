{ config, pkgs, lib, bleeding, stdenv, ... }:

let
  taffybarWithPackages = pkgs.taffybar.override {packages = p: with p; [safe]; };
in {
  imports = [
    ../packages/nixpkgs-from-submodule.nix
    ../packages/desktop-nagger.nix
    ../packages/haskell-packages.nix
    ../packages/python-packages.nix
    ../packages/xrandr-auto.nix
    ../packages/standard-linux-tools.nix
    ../roles/emacs.nix
    ../roles/nixops.nix
    ../roles/openvpn-client.nix
    ../users/binarin.nix
    ../packages/use-my-overlays.nix
    ../nixpkgs-proposed/nixos/modules/services/networking/epmd.nix
    ../nixpkgs-proposed/nixos/modules/services/amqp/rabbitmq.nix
  ];

  disabledModules = [ "services/amqp/rabbitmq.nix" ];
  services.epmd = {
    package = pkgs.proposed.erlang_nox;
  };
  services.rabbitmq = {
    enable = true;
    package = pkgs.proposed.rabbitmq-server.override {
      elixir = pkgs.proposed.elixir_1_6;
      erlang = pkgs.proposed.erlang_nox;
    };
    plugins = [ "rabbitmq_management" "rabbitmq_mqtt" ];
  };

  boot.supportedFilesystems = [ "exfat" ];
  nix.useSandbox = true;
  nix.extraOptions = ''
    gc-keep-outputs = true
    gc-keep-derivations = true
  '';

  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "naberius.binarin.ru";
      sshUser = "binarin";
      sshKey = "/root/.ssh/id_rsa";
      system = "x86_64-linux";
      maxJobs = 4;
      supportedFeatures = [ "kvm" ];
    }
  ];

  boot.kernel.sysctl."vm.swappiness" = 1;

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelModules = [ "fuse" ];

  networking.networkmanager.enable = true;

  networking.extraHosts = ''
    127.0.0.1 ${config.networking.hostName}
  '';

  networking.nat.enable = true;
  networking.nat.internalInterfaces = [
    # "lxc0"
    "virbr0"
  ];
  networking.nat.externalInterface = "wlp2s0";

  networking.bridges = {
    # lxc0 = {
    #   interfaces = [];
    # };
  };

  networking.interfaces = {
    # lxc0 = {
    #   ipv4.addresses = [
    #     { address = "10.10.30.1"; prefixLength = 24; }
    #   ];
    # };
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "UniCyr_8x16";
    consoleKeyMap = "dvp";
    defaultLocale = "ru_RU.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  standard-linux-tools.wireshark-package = pkgs.wireshark-gtk;

  programs.wireshark.enable = true;
  programs.wireshark.package = pkgs.wireshark-gtk;

  environment.systemPackages = let
    bleedingEdgePackages = with pkgs.bleeding; [
      alacritty
      gitAndTools.git-annex
    ];
    desktopPackages = with pkgs; [
      aspell
      aspellDicts.ru
      aspellDicts.en
      aspellDicts.nl
      audacious
      binarin-xrandr-auto
      # calibre
      chromium
      desktop-nagger
      dia
      dropbox
      dunst
      ebook_tools
      electrum
      evince
      firefox-beta-bin
      icewm # something to run in Xephyr
      icoutils
      imagemagickBig
      innoextract # for GOG games
      insync
      geeqie
      gimp-with-plugins
      gitg
      glxinfo
      google-chrome
      keepass
      libnotify
      lightdm # for dm-tool
      lightlocker
      mcomix
      mpc_cli
      mplayer
      networkmanagerapplet
      oblogout
      playerctl
      psi
      qt4 # for qtconfig
      shutter
      slack
      stack
      stalonetray
      steam
      taffybarWithPackages
      torbrowser
      tdesktop
      viber
      # wineFull
      workrave
      xdg-user-dirs
      xdotool
      xlsfonts
      xorg.xdpyinfo
      xorg.xev
      yandex-disk
    ];
    developmentPackages = with pkgs; [
      androidenv.platformTools
      ant
      apitrace
      arduino
      autoconf
      automake
      checkbashism
      electron
      elixir_1_6
      erlangR20
      fakeroot
      gcc
      git-review
      hugo
      lessc
      libvirt # for `vagrant plugin install vagrant-libvirt`
      libxslt # xsltproc - for building rabbitmq
      lsyncd
      mysql
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
    ];
    nixDevPackages = with pkgs; [
      patchelf
    ];
    utilityPackages = with pkgs; [
      gopass
      jekyll
      nfs-utils # for vagrant
      pdftk
      syncthing
      (texlive.combine {
        inherit (texlive) scheme-full beamer;
      })
      vagrant
      virt-viewer
      virtmanager
    ];
    otherPackages = with pkgs; [
      youtube-dl
      gnuplot
      gnome3.dconf
      gnome3.dconf-editor
      dmenu
      dmidecode
      gmrun
      haskellPackages.xmobar
      haskellPackages.yeganesh
      hsetroot
      isyncUnstable
      quasselClient
      keychain
      libreoffice
      mu
      pavucontrol
      skype
      wmctrl
      xclip
      xscreensaver
      xsel
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
     enableBluejeans = true;
     enableGoogleTalkPlugin = true;
     # enableAdobeFlash = true;
     jre = true;
     enableDjvu = true;
    };

    chromium = {
     # enablePepperFlash = true; # Chromium removed support for Mozilla (NPAPI) plugins so Adobe Flash no longer works
     enablePepperPDF = true;
    };
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      dejavu_fonts
      emacs-all-the-icons-fonts
      fira
      fira-code
      fira-mono
      font-awesome-ttf
      google-fonts
      inconsolata
      iosevka
      liberation_ttf
      mplus-outline-fonts
      noto-fonts
      powerline-fonts
      roboto
      roboto-mono
      roboto-slab
      source-code-pro
      terminus_font
      terminus_font_ttf
      ubuntu_font_family
      unifont
      vistafonts
    ];
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
  };

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    support32Bit = true;
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

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip pkgs.postscript-lexmark pkgs.epson-escpr ];

  services.xserver = {
    # videoDrivers = [ "ati_unfree" "ati" "noveau" "intel" "vesa" "vmware" "modesetting"];
    videoDrivers = ["modesetting" "ati"];
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

Section "InputClass"
  Identifier "Yubikey"
  MatchUSBID "1050:0403"
  Option "XkbModel" "pc104"
  Option "XkbLayout" "us"
  Option "XkbOptions" ""
  Option "XkbVariant" ""
EndSection

Section "InputClass"
    Identifier      "Marble Mouse"
    MatchUSBID      "046d:c408"
    Option          "SendCoreEvents" "true"
    Option "Buttons"            "9"
    Option "ButtonMapping"      "1 8 3 4 5 6 7 2 9"
    Option "EmulateWheel"       "true"
    Option "EmulateWheelButton" "9"
    Option "YAxisMapping"       "4 5"
    Option "XAxisMapping"       "6 7"
EndSection

# Section "Device"
#         Identifier  "Intel Graphics"
#         Driver      "intel"
#         Option      "TearFree" "true"
# EndSection
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

    multitouch = {
      enable = true;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = p: [ p.taffybar p.dbus p.monad-logger p.lens ];
    };

    windowManager.default = "xmonad";
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";

    desktopManager.gnome3.enable = true;
    libinput.enable = false;

    displayManager = {
      gdm.enable = lib.mkForce false;
      lightdm.enable = true;
    };
  };

  services.redshift = {
    enable = true;
    latitude = "52.3702";
    longitude = "4.8952";
    temperature = { day = 6500; night = 3000; };
    extraOptions = [ "-v" ];
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers = {
    root = {
      shell = "/run/current-system/sw/bin/zsh";
      subUidRanges = [ { startUid = 100001; count = 65534; } ];
      subGidRanges = [ { startGid = 1001; count = 999; } ];
    };
  };

  networking.firewall.enable = true;
  networking.firewall.checkReversePath = false; # I want my DHCP for VM's
  networking.firewall.allowedTCPPorts = [27036 27037];
  networking.firewall.allowedUDPPorts = [27031 27036];

  virtualisation = {
    # lxc.enable = true;
    # lxc.defaultConfig = ''
    #   lxc.network.type = veth
    #   lxc.network.link = lxc0
    # '';
    lxd.enable = true;
    docker.enable = true;
    docker.storageDriver = "overlay2";
    libvirtd.enable = true;
  };

  zramSwap = {
    enable = true;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";

  programs.ssh.startAgent = true;
  programs.light.enable = true;

  nix.binaryCaches = [
    "http://naberius.binarin.ru:5000"
    "https://cache.nixos.org"
    "https://nixcache.reflex-frp.org"
  ];

  nix.binaryCachePublicKeys = [
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    "naberius.binarin.ru-1:HeueNAbXuNomZp4xJL+ITAmJpSNYl/newnqoI85aUyc="
  ];

  systemd.services."binarin-auto-commit-wip" = let
    script = pkgs.writeScript "binarin-auto-commit-wip" ''
      #!${pkgs.bash}/bin/bash
      set -euo pipefail
      dirs="/etc/nixos /etc/nixos/nixpkgs-master /home/binarin/.rc"

      if [[ $(DISPLAY=:0 xprintidle-ng) -lt $((3600*1000)) ]]; then
        exit 0
      fi

      set -x

      for dir in $dirs; do
        if [[ -d "$dir" ]]; then
          cd "$dir"
          if [[ ! -z $(git status --porcelain) ]]; then
             git add .
             git commit -m "WIP $(date -R)"
             git push -f origin HEAD:refs/heads/wip-$(hostname)
          fi
        fi
      done
    '';
  in {
    description = "Automatically commit and push (to per-machine wip branches) in some important directories";
    path = [ pkgs.gitAndTools.gitFull pkgs.xprintidle-ng pkgs.bash pkgs.nettools ];
    serviceConfig = {
      Type = "oneshot";
      User = "binarin";
      ExecStart = script;
    };
  };
  systemd.timers."binarin-auto-commit-wip" = {
    description = "Periodically auto-commits/pushes to WIP branch some important repos";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "15min";
      OnUnitActiveSec = "30min";
    };
  };


  systemd.services.systemd-udev-settle.serviceConfig.ExecStart = ["" "${pkgs.coreutils}/bin/true"];

  # systemd.services.udev-monitor = {
  #   description="udev Monitoring";
  #   wantedBy=["sysinit.target"];
  #   after=["systemd-udevd-control.socket" "systemd-udevd-kernel.socket"];
  #   before=["sysinit.target" "systemd-udev-trigger.service" "systemd-udev-settle.service"];
  #   unitConfig = {
  #     DefaultDependencies = false;
  #   };
  #   wants=["systemd-udevd.service"];
  #   serviceConfig = {
  #     Type = "simple";
  #     ExecStart=''${pkgs.bash}/bin/bash -c "${pkgs.systemd}/bin/udevadm monitor --udev --env --kernel > /udev_monitor.log"'';
  #   };
  # };

  # boot.extraKernelParams = [
  #   "systemd.log_level=debug"
  #   "systemd.log_target=kmsg"
  #   "udev.log-priority=debug"
  #   "log_buf_len=8M"
  # ];

  systemd.services."binarin-org-sync" = let
    script = pkgs.writeScript "binarin-org-sync" ''
      #!${pkgs.bash}/bin/bash
      set -euo pipefail
      if [[ -f /home/binarin/org/push.sh ]]; then
        exec /home/binarin/org/push.sh
      fi
    '';
  in {
    description = "Syncs org-mode files via git";
    path = [ pkgs.gitAndTools.gitFull pkgs.wget pkgs.coreutils pkgs.bash ];
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

  services.compton = {
    enable = true;
    backend = "glx";
    vSync = "opengl-swc";
    package = pkgs.compton-git;
  };

  services.udev = let
    xrandrScript = pkgs.writeScript "xrandr-auto.sh" ''
        exec ${pkgs.binarin-xrandr-auto}/bin/xrandr-auto configure
    '';
  in {
    extraRules = ''
      KERNEL=="card0", SUBSYSTEM=="drm", ACTION=="change", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/binarin/.Xauthority", RUN+="${pkgs.bash}/bin/bash ${xrandrScript}"
    '';
  };
  services.postgresql = {
    enable = true;
    initialScript = pkgs.writeText "postgres-init" ''
      create user binarin with
        unencrypted password 'aoeuaoeu'
        ;
      create database toshl with
        owner = binarin
        encoding = UTF8
        lc_collate = "ru_RU.UTF-8"
        lc_ctype = "ru_RU.UTF-8"
        ;

    '';
  };

  systemd.user.services.taffybar = {
    description = "taffybar (with monitor autodetection)";
    path = [ taffybarWithPackages ];
    wantedBy = [ "default.target" ];
    restartIfChanged = true;
    serviceConfig = {
      Type = "simple";
      ExecStart = pkgs.writeScript "taffybar-restarter" ''
        #!${pkgs.bash}/bin/bash
        set -x
        ${pkgs.binarin-xrandr-auto}/bin/xrandr-auto get-primary
        primary=$?
        exec ${taffybarWithPackages}/bin/taffybar $primary
      '';
      Restart = "always";
      RestartSec = "2";
      StartLimitIntervalSec = "0";
    };
  };

  services.tlp.enable = true;
  services.bitlbee = {
    enable = true;
    authMode = "Closed";
    plugins = [ pkgs.bitlbee-facebook ];
    extraSettings = ''
      AuthPassword = md5:CiSC/UbtoWs9zOUxB6H7HBRc+Lwn
      OperPassword = md5:CiSC/UbtoWs9zOUxB6H7HBRc+Lwn
    '';
  };

}
