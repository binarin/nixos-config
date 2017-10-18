{ config, pkgs, lib, bleeding, stdenv, ... }:

let
  taffybarWithPackages = pkgs.taffybar.override {packages = p: with p; [safe]; };
  xrandrShellHelpers = pkgs.writeText "xrandr-shell-helpers.sh" ''
        export PATH=$PATH\${PATH:+:}${pkgs.gnugrep}/bin:${pkgs.xorg.xrandr}/bin:${pkgs.procps}/bin

        HDMI_STATUS=
        if xrandr | grep -q 'HDMI-1 connected'; then
            HDMI_STATUS=connected
        fi
        DP1_1_STATUS=
        if xrandr | grep -q 'DP-1-1 connected'; then
            DP1_1_STATUS=connected
        fi
        DP1_3_STATUS=
        if xrandr | grep -q 'DP-1-3 connected'; then
            DP1_3_STATUS=connected
        fi
  '';
in {
  # powerManagement.enable = true;
  imports = [
    ../packages/nixpkgs-from-submodule.nix
    ../packages/bleeding-edge.nix
    ../packages/desktop-nagger.nix
    ../packages/haskell-packages.nix
    ../packages/python-packages.nix
#    ../packages/perl-packages.nix
    ../packages/standard-linux-tools.nix
    ../roles/emacs.nix
  ];

  boot.supportedFilesystems = [ "exfat" ];
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
  networking.nat.internalInterfaces = ["lxc0" "virbr0"];
  networking.nat.externalInterface = "wlp2s0";

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
  time.timeZone = "Europe/Amsterdam";

  standard-linux-tools.wireshark-package = pkgs.wireshark-gtk;

  environment.systemPackages = let
    bleedingEdgePackages = with pkgs.bleeding; [
      dosbox
      firefox
      alacritty
      # google-chrome-beta
      # google-chrome-dev
      # wineFull
      # winetricks
    ];
    desktopPackages = with pkgs; [
      aspell
      aspellDicts.ru
      aspellDicts.en
      audacious
      chromium
      desktop-nagger
      dia
      dunst
      ebook_tools
      electrum
      evince
      icewm # something to run in Xephyr
      icoutils
      imagemagickBig
      innoextract # for GOG games
      geeqie
      gimp-with-plugins
      gitg
      glxinfo
      keepass
      libnotify
      lightdm # for dm-tool
      lightlocker
      mpc_cli
      mplayer
      networkmanagerapplet
      oblogout
      playerctl
      shutter
      slack
      stack
      stalonetray
      taffybarWithPackages
      torbrowser
      tdesktop
      viber
      # wineFull
      workrave
      xdg-user-dirs
      xlsfonts
      xorg.xbacklight
      xorg.xdpyinfo
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
      electron
      pkgs.bleeding.elixir_1_5
      erlangR19
      fakeroot
      gcc
      git-review
      go_1_7
      libvirt # for `vagrant plugin install vagrant-libvirt`
      libxslt # xsltproc - for building rabbitmq
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
      nix-repl
      nixops
      patchelf
    ];
    utilityPackages = with pkgs; [
      dropbox-cli
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
      conkeror
      gnome3.dconf
      gnome3.dconf-editor
      dmenu
      dmidecode
      gmrun
      haskellPackages.xmobar
      haskellPackages.yeganesh
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
     enableAdobeFlash = true;
     jre = true;
     enableDjvu = true;
    };

    chromium = {
     enablePepperFlash = true; # Chromium removed support for Mozilla (NPAPI) plugins so Adobe Flash no longer works
     enablePepperPDF = true;
    };

    packageOverrides = super: rec {
      linuxPackages_latest = super.linuxPackages_latest.extend (linuxSelf: linuxSuper: {
        evdi = linuxSuper.evdi.overrideAttrs (oldAttrs: {
          name = "evdi-unstable";
          src = pkgs.fetchFromGitHub {
            owner = "DisplayLink";
            repo = "evdi";
            rev = "ee1c578774e62fe4b08d92750620ed3094642160";
            sha256 = "1m3wkmw4hjpjax7rvhmpicz09d7vxcxklq797ddjg6ljvf12671b";
          };
        });
      });
      displaylink = super.callPackage ../packages/displaylink.nix {
        inherit (pkgs.linuxPackages_latest) evdi; # doesn't matter which version, it'll be overriden by module
      };
      xkbvalidate = super.xkbvalidate.override { libxkbcommon = libxkbcommon_dvp; };
      libxkbcommon_dvp = super.libxkbcommon.overrideAttrs (oldAttrs: {
         configureFlags = [
           "--with-xkb-config-root=${xorg.xkeyboard_config_dvp}/etc/X11/xkb"
           "--with-x-locale-root=${xorg.libX11.out}/share/X11/locale"
         ];
      });
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

  hardware.opengl = let
    custom-mesa = pkgs.mesa_noglu.override {
      enableTextureFloats = true;
    };
  in {
    enable = true;
    driSupport32Bit = true;
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
  services.printing.drivers = [ pkgs.hplip pkgs.postscript-lexmark ];

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

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = p: [ p.taffybar p.dbus p.monad-logger ];
    };

    windowManager.default = "xmonad";
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";

    desktopManager.gnome3.enable = true;
    libinput.enable = false;

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
  programs.zsh.ohMyZsh = {
    enable = true;
    plugins = [
      "autojump"
      "cabal"
      "coloredman"
      "compleat"
      "cpanm"
      "deb"
      "debian"
      "git"
      "perl"
    ];
    theme = "nebirhos";
  };
  programs.bash.enableCompletion = true;

  # ghcjs
  nix.trustedBinaryCaches = [ "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

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

  systemd.services."binarin-mail-sync" = let
    script = pkgs.writeScript "binarin-mail-sync" ''
      #!${pkgs.bash}/bin/bash
      set -euo pipefail
      if [[ ! -f $HOME/.work-offline ]]; then
        mbsync -a
      fi
    '';
  in {
    description = "Sync my e-mails to local storage";
    path = [ pkgs.isyncUnstable ];
    serviceConfig = {
      Type = "oneshot";
      User = "binarin";
      ExecStart = script;
    };
  };
  systemd.timers."binarin-mail-sync" = {
    description = "Periodically syncs mail to local storage";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "10min";
      OnUnitActiveSec = "10min";
    };
  };

  security.pam.loginLimits = [
    {
      domain = "*";
      type = "-";
      item = "nofile";
      value = "131072";
    }
  ];

  services.compton = {
    enable = true;
    backend = "xr_glx_hybrid";
    vSync = "opengl-swc";
    package = pkgs.bleeding.compton-git;
  };

  services.udev = let
    xrandrScript = pkgs.writeScript "xrandr-auto.sh" ''
        exec > /tmp/xr 2>&1
        set -uxo pipefail
        . ${xrandrShellHelpers}

        xrandr

        if [[ connected == $DP1_1_STATUS && connected == $DP1_3_STATUS ]]; then
            xrandr --output DP-1-1 --auto --primary
            xrandr --output DP-1-3 --auto --left-of DP-1-1
            xrandr --output eDP-1 --auto --right-of DP-1-1
        elif [[ connected == $HDMI_STATUS ]]; then
            xrandr --output HDMI-1 --auto --primary --right-of eDP-1
        else
            xrandr --output HDMI-1 --off
            xrandr --output DP-1-1 --off
            xrandr --output DP-1-3 --off
        fi
        xrandr
        pkill -f taffybar
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
        . ${xrandrShellHelpers}
        monitor=0
        if [[ connected == "$HDMI_STATUS" ]]; then
          monitor=1
        elif [[ connected == "$DP1_1_STATUS" && connected == "$DP1_3_STATUS" ]]; then
          monitor=1
        fi
        exec taffybar $monitor
      '';
      Restart = "always";
    };
  };

  services.mopidy = {
    enable = true;
    extensionPackages = [ pkgs.mopidy-gmusic pkgs.mopidy-youtube ];
    extraConfigFiles = [ "/etc/mopidy.conf" ];
    configuration = ''
      [audio]
      output = pulsesink server=127.0.0.1
    '';
  };
}
