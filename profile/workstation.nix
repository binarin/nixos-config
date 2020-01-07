{ config, pkgs, lib, bleeding, stdenv, ... }:

let
  taffybarWithPackages = pkgs.taffybar.override {packages = p: with p; [safe]; };
  taffybarWrapped = pkgs.stdenv.mkDerivation {
    name = "taffybar-with-packages-and-theme";
    buildInputs = [ pkgs.wrapGAppsHook pkgs.gnome3.adwaita-icon-theme pkgs.gnome2.gnome_icon_theme pkgs.hicolor-icon-theme pkgs.networkmanagerapplet pkgs.gnome-themes-extra ];
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      cd $out/bin
      cp -s ${taffybarWithPackages}/bin/taffybar .
    '';
  };
  nmappletWrapped = pkgs.stdenv.mkDerivation {
    name = "nmapplet-with-themes";
    buildInputs = [ pkgs.wrapGAppsHook pkgs.gnome3.adwaita-icon-theme pkgs.gnome2.gnome_icon_theme pkgs.hicolor-icon-theme pkgs.gnome-themes-extra ];
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
    ../packages/haskell-packages.nix
    # ../packages/python-packages.nix

    ../profile/emacs.nix
    ../profile/nixops.nix
    ../profile/openvpn-client.nix

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
      "https://cache.nixos.org"
      "https://nixcache.reflex-frp.org"
    ];

    binaryCachePublicKeys = [
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    ];

    useSandbox = true;

    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
    '';
  };

  networking = {
    networkmanager = {
      enable = true;
      dispatcherScripts = [
        {
          source = pkgs.writeScript "upHook" ''
            #!${pkgs.bash}/bin/bash
            export PATH=$PATH:${pkgs.unixtools.getent}/bin:${pkgs.gawk}/bin

            if [[ "$CONNECTION_ID" == "BK-GUEST" ]] ; then
              AMON_IP=$(getent hosts amon.binarin.ru | awk '{ print $1 }')
              if [[ -z $AMON_IP ]]; then
                AMON_IP=82.197.211.232
                logger "amon.binarin.ru failed to resolve, defaulting to $AMON_IP"
              fi
              if [[ "$NM_DISPATCHER_ACTION" == "up" ]]; then
                logger "BK-G UP: Adding static route to amon $AMON_IP via $IP4_GATEWAY"
                ip r a $AMON_IP via $IP4_GATEWAY || true
                systemctl start openvpn-udp-to-lanfear.service || true
              elif [[ "$NM_DISPATCHER_ACTION" == "down" ]]; then
                logger "BK-G DOWN: Deleting static route to amon $AMON_IP"
                ip r d $AMON_IP via $IP4_GATEWAY || true
                systemctl stop openvpn-udp-to-lanfear.service || true
              fi
            fi
          '';
        }
      ];
    };
    resolvconf.dnsExtensionMechanism = false;
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

  standard-linux-tools.wireshark-package = pkgs.wireshark-qt;

  programs.wireshark.enable = true;
  programs.wireshark.package = pkgs.wireshark-qt;
  programs.chromium.enable = true;

  environment.systemPackages = let
  bleedingEdgePackages = with pkgs.bleeding; [
      firefox-bin
      goldendict
      kubernetes
      pythonPackages.pywatchman
      openscad
      simplescreenrecorder
      vscode
    ];
    desktopPackages = with pkgs; [
      alacritty
      anki
      aspell
      aspellDicts.ru
      aspellDicts.en
      aspellDicts.nl
      audacious
      binarin-xrandr-auto
      bitwarden-cli
      blender
      # calibre
      chromium
      # desktop-nagger
      desktop-file-utils
      dia
      # dropbox
      dunst
      ebook_tools
      electrum
      evince
      freecad
      icewm # something to run in Xephyr
      icoutils
      imagemagickBig
      innoextract # for GOG games
      insync
      geeqie
      gimp-with-plugins
      # gitg
      glxinfo
      gnome2.gnome_icon_theme
      gnome3.adwaita-icon-theme
      google-chrome
      hicolor-icon-theme
      k2pdfopt
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
      sweethome3d.application
      taffybarWrapped
      #torbrowser
      tdesktop
      # viber
      # wineFull
      workrave
      xdg-user-dirs
      xdotool
      xlsfonts
      xorg.xdpyinfo
      xorg.xev
      # yandex-disk
    ];
    developmentPackages = with pkgs; [
      # androidenv.platformTools
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
      nodejs-10_x
      ncurses
      # oraclejdk8
      pkgconfig
      python35Packages.virtualenv
      quilt
      sbcl
      subversion
      tightvnc
      travis
      watchman
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
      # isyncUnstable
      quasselClient
      keychain
      libreoffice
      mu
      pavucontrol
      # skype
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

    oraclejdk.accept_license = true;

    firefox = {
     enableBluejeans = true;
     enableGoogleTalkPlugin = true;
     # jre = true;
     enableDjvu = true;
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
  Option "TransformationMatrix" "0 1 0 -1 0 1 0 0 1"
  Option "MiddleEmulation" "True"
	Option "Emulate3Buttons" "True"
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

Section "Device"
        Identifier  "Intel Graphics"
        Driver      "intel"
        Option      "TearFree" "true"
EndSection
    '';
    enable = true;
    layout = "us,ru";

    xkbDir = "${pkgs.xorg.xkeyboardconfig_dvp}/share/X11/xkb";
    xkbVariant = "dvp,dvp";
    xkbOptions = "grp:menu_toggle,ctrl:nocaps,altwin:super_win,grp:sclk_toggle,ctrl:ralt_rctrl";

    libinput = {
      enable = true;
      clickMethod = "none";
      middleEmulation = true;
      tapping = false;
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

    displayManager = {
      gdm.enable = lib.mkForce false;
      lightdm.enable = true;
    };
  };

  location.latitude = 52.3702;
  location.longitude = 4.8952;

  services.redshift = {
    enable = true;
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

  # Because systemd doesn't enforce resource limits on user units
  systemd.services."kill-leaking-taffybar" = let
    script = pkgs.writeScript "kill-leaking-taffybar" ''
      #!${pkgs.bash}/bin/bash
      taffy_pid=$(pgrep -f taffybar-linux)
      if [[ -n $taffy_pid ]]; then
        taffy_size=$(ps h -eo rss -q $taffy_pid)
        if [[ $taffy_size -gt 240000 ]]; then
          kill $taffy_pid
        fi
      fi
    '';
  in {
    description = "Kills leaking taffybar";
    path = with pkgs; [ procps ];
    serviceConfig = {
      Type = "oneshot";
      User = "binarin";
      ExecStart = script;
    };
  };
  systemd.timers."kill-leaking-taffybar" = {
    description = "Periodically updates org-mode files via git";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "5min";
      OnUnitActiveSec = "5min";
    };
  };

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

  # systemd.services."binarin-mail-sync" = let
  #   script = pkgs.writeScript "binarin-mail-sync" ''
  #     #!${pkgs.bash}/bin/bash
  #     set -euo pipefail
  #     if [[ ! -f $HOME/.work-offline ]]; then
  #       mbsync -a
  #     fi
  #   '';
  # in {
  #   description = "Sync my e-mails to local storage";
  #   path = [ pkgs.isyncUnstable ];
  #   serviceConfig = {
  #     Type = "oneshot";
  #     User = "binarin";
  #     ExecStart = script;
  #   };
  # };
  # systemd.timers."binarin-mail-sync" = {
  #   description = "Periodically syncs mail to local storage";
  #   wantedBy = [ "timers.target" ];
  #   timerConfig = {
  #     OnBootSec = "10min";
  #     OnUnitActiveSec = "10min";
  #   };
  # };

  services.compton = {
    enable = true;
    backend = "glx";
    vSync = true;
    # package = pkgs.compton-git;
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
    # path = [ ];
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
        exec ${taffybarWrapped}/bin/taffybar +RTS
      '';
      Restart = "always";
      RestartSec = "2";
      MemoryAccounting = "yes";
      MemoryMax = "10M";
    };
  };

  # TLP brings you the benefits of advanced power management for Linux without the need to understand every technical detail.
  services.tlp.enable = true;

  services.logind = {
    lidSwitch = "suspend";
    lidSwitchExternalPower = "ignore";
    lidSwitchDocked = "ignore";
  };
}