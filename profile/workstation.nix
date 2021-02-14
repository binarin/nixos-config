{ config, pkgs, lib, bleeding, stdenv, ... }:

let
in {
  imports = [
    ../packages/use-my-overlays.nix
    ../packages/standard-linux-tools.nix
    ../packages/haskell-packages.nix
    ../packages/user-packages.nix

    ../profile/emacs.nix

    ../users/binarin.nix
  ];

  boot = {
    supportedFilesystems = [ "exfat" "nfs" "cifs" ];
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
      dns = "dnsmasq";
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
      flameshot
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
      v4l-utils
      bluejeans-gui
      youtube-music-desktop-app
      gnome-icon-theme
      hicolor-icon-theme
      gnome3.adwaita-icon-theme
      gnome2.gnome_icon_theme
      gnome-themes-extra
      zafiro-icons
      networkmanagerapplet
      usbutils.python
      xorg.xf86inputlibinput
      wally-cli
      my-xmonad-config
      graphviz
      firefox-bin
      goldendict
      openscad
      vscode
      pkgs.bleeding.idea.idea-community
      openconnect_pa
      prusa-slicer
      anki
      aspell
      aspellDicts.ru
      aspellDicts.en
      aspellDicts.nl
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
      borgbackup
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
    gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
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

    # libinput = {
    #   enable = lib.mkForce false;
    #   clickMethod = "none";
    #   middleEmulation = true;
    #   tapping = false;
    # };

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

  services.udev.extraRules = let script = pkgs.writeShellScript "set-camera-zoom" ''
    ${pkgs.v4l-utils}/bin/v4l2-ctl -c zoom_absolute=180
    ${pkgs.v4l-utils}/bin/v4l2-ctl -c pan_absolute=10800
  '';
  in ''SUBSYSTEM=="usb", ATTR{idVendor}=="046d", ATTR{idProduct}=="0892", RUN+="${script}"'';

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
}
