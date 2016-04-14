# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [ { name = "ishamael-crypt"; device="/dev/disk/by-partlabel/ishamael-crypt"; allowDiscards = true; } ];

  boot.kernel.sysctl."vm.swappiness" = 1;

  networking.hostName = "ishamael"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

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
    chromium
    dmenu
    dmidecode
    emacs25pre
    erlang
    firefoxWrapper
    gitAndTools.gitFull
    gmrun
    haskellPackages.xmobar
    haskellPackages.yeganesh
    iptables
    gnumake
    manpages
    nmap
    pciutils
    rxvt_unicode
    screen
    sudo
    tcpdump
    tmux
    usbutils
    wget
    wmctrl
    workrave
    xclip
    xlsfonts
    xscreensaver
    zsh
  ];

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

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;
  services.xserver = {
    enable = true;
    layout = "us,ru";
    xkbVariant = "dvp,winkeys";
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
  users.extraUsers.binarin = {
    description = "Alexey Lebedeff";
    uid = 1000;
    isNormalUser = true;
    shell = "/run/current-system/sw/bin/zsh";    
    extraGroups = [ "networkmanager" "docker" "libvirtd" ];
  };

  security.sudo.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs";
  virtualisation.libvirtd.enable = true;


  zramSwap = {
    enable = true;
  };


  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

}
