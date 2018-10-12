{ config, pkgs, lib, bleeding, stdenv, ... }:

let
  taffybarWithPackages = pkgs.taffybar.override {packages = p: with p; [safe]; };
  taffybarWrapped = pkgs.stdenv.mkDerivation {
    name = "taffybar-with-packages-and-theme";
    buildInputs = [ pkgs.wrapGAppsHook pkgs.gnome3.adwaita-icon-theme pkgs.gnome2.gnome_icon_theme pkgs.hicolor-icon-theme ];
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      cd $out/bin
      cp -s ${taffybarWithPackages}/bin/taffybar .
    '';
  };
  nmappletWrapped = pkgs.stdenv.mkDerivation {
    name = "nmapplet-with-themes";
    buildInputs = [ pkgs.wrapGAppsHook pkgs.gnome3.adwaita-icon-theme pkgs.gnome2.gnome_icon_theme pkgs.hicolor-icon-theme ];
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      cd $out/bin
      cp -s ${pkgs.networkmanagerapplet}/bin/nm-applet .
    '';
  };

in {
  imports = [
    ../packages/use-my-overlays.nix

    ../packages/desktop-nagger.nix
    ../packages/xrandr-auto.nix
    ../packages/standard-linux-tools.nix
    # ../packages/haskell-packages.nix
    # ../packages/python-packages.nix

    ../roles/emacs.nix
    ../roles/nixops.nix
    ../roles/openvpn-client.nix

    ../users/binarin.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = [ "exfat" ];
    kernelModules = [ "fuse" ];
    kernel.sysctl."vm.swappiness" = 1;
  };

  nix = {
    binaryCaches = [
      "http://naberius.binarin.ru:5000"
      "https://cache.nixos.org"
      "https://nixcache.reflex-frp.org"
    ];

    binaryCachePublicKeys = [
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "naberius.binarin.ru-1:HeueNAbXuNomZp4xJL+ITAmJpSNYl/newnqoI85aUyc="
    ];

    useSandbox = true;

    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
    '';
  };

  networking = {
    networkmanager.enable = true;
    extraHosts = ''
      127.0.0.1 ${config.networking.hostName}
    '';
    nat = {
      enable = true;
      internalInterfaces = [
        "virbr0"
      ];
      externalInterface = "wlp2s0";
    };
  };

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
      gnome2.gnome_icon_theme
      gnome3.adwaita-icon-theme
      google-chrome
      hicolor-icon-theme
      keepass
      libnotify
      lightdm # for dm-tool
      lightlocker
      mcomix
      mpc_cli
      mplayer
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
     jre = true;
     enableDjvu = true;
    };

    chromium = {
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

    xkbDir = "${pkgs.xorg.xkeyboardconfig_dvp}/share/X11/xkb";
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
    docker.enable = true;
    docker.storageDriver = "overlay2";
    libvirtd.enable = true;
  };

  zramSwap = {
    enable = true;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.09";

  programs.ssh.startAgent = true;
  programs.light.enable = true;
  programs.gnupg.agent.enable = true;

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

  # XXX Try disabling, maybe already fixed
  systemd.services.systemd-udev-settle.serviceConfig.ExecStart = ["" "${pkgs.coreutils}/bin/true"];

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
    packages = [ pkgs.crda ];
    extraRules = ''
      KERNEL=="card0", SUBSYSTEM=="drm", ACTION=="change", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/binarin/.Xauthority", RUN+="${pkgs.bash}/bin/bash ${xrandrScript}"
    '';
  };

  systemd.user.services.status-notifier-watcher = {
    description = "https://www.freedesktop.org/wiki/Specifications/StatusNotifierItem/StatusNotifierWatcher/";
    wantedBy = [ "default.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${(pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.status-notifier-item)}/bin/status-notifier-watcher";
    };
  };

  systemd.user.services.nm-applet = {
    description = "https://www.freedesktop.org/wiki/Specifications/StatusNotifierItem/StatusNotifierWatcher/";
    after = [ "status-notifier-watcher.service "];
    wants = [ "status-notifier-watcher.service "];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${nmappletWrapped}/bin/nm-applet --indicator";
    };
  };

  systemd.user.services.taffybar = {
    description = "taffybar (with monitor autodetection)";
    path = [ taffybarWithPackages ];
    after = [ "status-notifier-watcher.service "];
    wants = [ "status-notifier-watcher.service "];
    unitConfig = {
        StartLimitIntervalSec = "0";
    };

    restartIfChanged = true;
    serviceConfig = {
      Type = "simple";
      ExecStart = pkgs.writeScript "taffybar-restarter" ''
        #!${pkgs.bash}/bin/bash
        exec ${taffybarWrapped}/bin/taffybar
      '';
      Restart = "always";
      RestartSec = "2";
    };
  };

  # TLP brings you the benefits of advanced power management for Linux without the need to understand every technical detail.
  services.tlp.enable = true;
}
