{ config, pkgs, ... }:

{
  boot.kernel.sysctl."vm.swappiness" = 1;

  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "UniCyr_8x16";
    consoleKeyMap = "dvp";
    defaultLocale = "ru_RU.UTF-8";
  };
  # Set your time zone.
  time.timeZone = "Europe/Moscow";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # Some music
    audacious
    # strings
    binutils
    bridge-utils
    chromium
    cryptsetup
    gnome3.dconf
    gnome3.dconf-editor
    debootstrap
    dmenu
    dmidecode
    emacs25pre
    erlang
    file
    firefox
    gitAndTools.gitFull
    gmrun
    haskellPackages.xmobar
    haskellPackages.yeganesh
    iptables
    isyncUnstable
    gnumake
    gnupg1orig
    gnupg
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
    rxvt_unicode
    screen
    skype
    sudo
    tcpdump
    tmux
    usbutils
    which
    wget
    wmctrl
    workrave
    xclip
    xlsfonts
    xscreensaver
    zsh
  ];

  nixpkgs.config = {

    allowUnfree = true;

    firefox = {
     enableGoogleTalkPlugin = true;
     enableAdobeFlash = true;
    };

    chromium = {
     enablePepperFlash = true; # Chromium removed support for Mozilla (NPAPI) plugins so Adobe Flash no longer works
     enablePepperPDF = true;
    };

    packageOverrides = super: {
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
      mplus-outline-fonts
      terminus_font
      corefonts
      inconsolata
      ubuntu_font_family
      unifont
    ];
  };

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  # List services that you want to enable:

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
	Identifier "CirqueTouchpad"
	MatchProduct "GlidePoint"
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
      extraGroups = [ "networkmanager" "docker" "libvirtd" "wheel" ];
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

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
    lxc.enable = true;
    lxc.defaultConfig = ''
      lxc.network.type = veth
      lxc.network.link = virbr0
      lxc.network.flags = up
      lxc.aa_profile = unconfined
    '';
  };

  zramSwap = {
    enable = true;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

  programs.zsh.enable = true;
  programs.bash.enableCompletion = true;
}
