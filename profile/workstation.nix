{ config, pkgs, lib, bleeding, stdenv, ... }:

let
  taffybarWithPackages = pkgs.taffybar.override {packages = p: with p; [safe]; };
  taffybarWrapped = pkgs.stdenv.mkDerivation {
    name = "taffybar-with-packages-and-theme";
    buildInputs = [ pkgs.wrapGAppsHook pkgs.gnome3.adwaita-icon-theme pkgs.gnome2.gnome_icon_theme pkgs.hicolor-icon-theme nmappletWrapped pkgs.gnome-themes-extra ];
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
    ../packages/xrandr-auto.nix
    ../packages/standard-linux-tools.nix
    ../packages/haskell-packages.nix
    ../packages/user-packages.nix

    ../profile/emacs.nix

    ../users/binarin.nix
  ];

  boot = {
    supportedFilesystems = [ "exfat" ];
    kernelModules = [ "fuse" "v4l2loopback" ];
    extraModprobeConfig = ''
      options v4l2loopback video_nr=7
      options v4l2loopback card_label="video loopback"
    '';
    extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
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

  nixpkgs.config = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };

  networking = {
    networkmanager = {
      enable = true;
    };
    resolvconf.dnsExtensionMechanism = false;
    extraHosts = ''
      127.0.0.1 ${config.networking.hostName}
      0.0.0.0 pikabu.ru leprosorium.ru shovinist.leprosorim.ru idiod.leprosorium.ru games.leprosorium.ru meduza.io
    '';
    nat = {
      enable = true;
      internalInterfaces = [
        "virbr0"
      ];
      externalInterface = "wlp2s0";
    };
  };

  console.font = "UniCyr_8x16";

  i18n = {
    defaultLocale = "ru_RU.UTF-8";
  };

  time.timeZone = "Europe/Amsterdam";

  programs.wireshark.enable = true;
  programs.wireshark.package = pkgs.wireshark-qt;

  programs.chromium.enable = true;
  programs.chromium.extraOpts = {
    ExternalProtocolDialogShowAlwaysOpenCheckbox = true;
  };

  userPackages = let
    bleedingEdgePackages = with pkgs.bleeding; [
      looking-glass-client
      protonvpn-cli
    ];
    developmentPackages = with pkgs; [
      autoconf
      automake
      gcc
      godot
      hugo
      kubernetes
      leiningen
      lsyncd
      pkgconfig
      sbt
    ];
    desktopPackages = with pkgs; [
      nmappletWrapped
      taffybarWrapped
      usbutils.python
      xorg.xf86inputlibinput
      wally-cli
      my-xmonad-config
      graphviz
      firefox-bin
      goldendict
      openscad
      vscode
      #idea.idea-community
      (idea.idea-community.overrideAttrs (oldAttrs: rec {
        name = "idea-community-${version}";
        version = "2020.1.4"; /* updated by script */
        src = fetchurl {
          url = "https://download.jetbrains.com/idea/ideaIC-${version}.tar.gz";
          sha256 = "155xiv1d39c4wkm7zqv3f4ajhlrylbyfx2xrzs1r57ippcp54rhg"; /* updated by script */
        };
      }))
      openconnect_pa
      prusa-slicer
      anki
      aspell
      aspellDicts.ru
      aspellDicts.en
      aspellDicts.nl
      binarin-xrandr-auto
      chromium
      desktop-file-utils
      dunst
      evince
      freecad
      imagemagickBig
      geeqie
      gimp
      gitg
      glxinfo
      google-chrome
      google-cloud-sdk
      hicolor-icon-theme
      libnotify
      mplayer
      escrotum
      slack
      stack
      stalonetray
      tdesktop
      skype
      workrave
      xdg-user-dirs
      xdotool
      xlsfonts
      xorg.xdpyinfo
      xorg.xev
    ];
    in bleedingEdgePackages ++ desktopPackages ++ developmentPackages;


  environment.systemPackages = let
    nixDevPackages = with pkgs; [
      patchelf
    ];
    utilityPackages = with pkgs; [
      pbzip2
      pigz
      ntfs3g
      gopass
      jekyll
      pdftk
      syncthing
      (texlive.combine {
        inherit (texlive) scheme-full beamer;
      })
      # vagrant
      virt-viewer
      virtmanager
    ];
    otherPackages = with pkgs; [
      youtube-dl
      gnuplot
      gnome3.dconf
      gnome3.dconf-editor
      gnome3.gnome-tweaks
      dmenu
      dmidecode
      gmrun
      haskellPackages.xmobar
      haskellPackages.yeganesh
      hsetroot
      quasselClient
      keychain
      libreoffice
      mu
      pavucontrol
      wmctrl
      xclip
      xscreensaver
      xsel
    ];
  in otherPackages ++
     nixDevPackages ++
     utilityPackages;


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
    extraModules = [ pkgs.pulseaudio-modules-bt ];
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
    modules = [ pkgs.xorg.xf86inputlibinput ];
    videoDrivers = [ "amdgpu" "modesetting" ];
    config = ''
Section "Device"
     Identifier "AMD"
     Driver "amdgpu"
     Option "VariableRefresh" "true"
     Option "TearFree" "on"
EndSection

Section "InputClass"
	Identifier "CirqueTouchpad1"
	MatchProduct "GlidePoint"
	Option "SwapAxes" "True"
	Option "Emulate3Buttons" "True"
	Option "InvertY" "True"
  Option "Tapping" "False"
  Option "TappingDrag" "False"
EndSection

Section "InputClass"
	Identifier "CirqueTouchpad2"
  MatchUSBID "0488:0280"
  Option "TransformationMatrix" "0 1 0 -1 0 1 0 0 1"
  Option "MiddleEmulation" "True"
	Option "Emulate3Buttons" "True"
  Option "Tapping" "False"
  Option "TappingDrag" "False"
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
    Driver          "libinput"
    Option "ScrollMethod" "button"
    Option "ScrollButton" "8"
    Option "MiddleEmulation" "on"
EndSection

Section "Device"
        Identifier  "Intel Graphics"
        Driver      "intel"
        Option      "TearFree" "true"
EndSection
    '';
    enable = true;
    layout = "us,ru";

    # xkbDir = "${pkgs.xorg.xkeyboardconfig_dvp}/share/X11/xkb";
    xkbVariant = ",winkeys";
    xkbOptions = "grp:menu_toggle,ctrl:nocaps,altwin:super_win,grp:sclk_toggle,ctrl:ralt_rctrl";

    libinput = {
      enable = lib.mkForce false;
      clickMethod = "none";
      middleEmulation = true;
      tapping = false;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    desktopManager.xterm.enable = false;

    desktopManager.gnome3.enable = true;
    # desktopManager.plasma5.enable = true;
    # desktopManager.xfce.enable = true;

    displayManager = {
      gdm.enable = lib.mkForce false;
      lightdm.enable = true;
      defaultSession = "none+xmonad";
    };
  };

  location.latitude = 52.3702;
  location.longitude = 4.8952;

  users.extraUsers = {
    root = {
      shell = "/run/current-system/sw/bin/zsh";
    };
  };

  networking.firewall.enable = true;
  networking.firewall.checkReversePath = false; # I want my DHCP for VM's
  networking.firewall.allowedTCPPorts = [27036 27037];
  networking.firewall.allowedUDPPorts = [27031 27036];

  virtualisation = {
    docker.enable = true;
    # docker.storageDriver = "overlay2";
    libvirtd.enable = true;
  };

  zramSwap = {
    enable = true;
  };

  programs.java = {
    enable = true;
    package = pkgs.openjdk11;
  };

  programs.ssh.startAgent = true;
  programs.light.enable = true;
  programs.gnupg.agent.enable = true;

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
    vSync = true;
  };

  services.udev = let
    xrandrScript = pkgs.writeScript "xrandr-auto.sh" ''
        exec ${pkgs.binarin-xrandr-auto}/bin/xrandr-auto configure
    '';
  in {
    packages = [ pkgs.crda pkgs.pulseaudioFull ];
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

  # TLP brings you the benefits of advanced power management for Linux without the need to understand every technical detail.
  services.tlp.enable = true;

  services.logind = {
    lidSwitch = "suspend";
    lidSwitchExternalPower = "ignore";
    lidSwitchDocked = "ignore";
  };

  security.wrappers = {
    fbterm = {
      source = "${pkgs.fbterm}/bin/fbterm";
      owner   = "nobody";
      group   = "nogroup";
      capabilities = "cap_sys_tty_config+ep";
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
}
