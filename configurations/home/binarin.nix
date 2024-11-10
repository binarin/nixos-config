{ flake, pkgs, lib, system, config, nixosConfig, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    self.homeModules.default
    "${self}/users/binarin-hm.nix"
    "${self}/users/binarin-hm-linux.nix"
    inputs.sops-nix.homeManagerModules.sops
  ];

  sops.secrets.hass-mqtt-password = {
    path = "%r/hass-mqtt-password";
  };

  services.lnxlink = {
    enable = true;
    logLevel = "DEBUG";
    mqtt = {
      user = "valak";
      clientId = "valak";
      server = "192.168.2.23";
      passwordFile = "%t/hass-mqtt-password";
    };
    addons = {
      cpu.enable = true;
      battery.enable = true;
      audio_select.enable = true;
    };
  };

  home.stateVersion = lib.mkDefault "24.05";

  my.programs.emacs.enable = true;
  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
    theme = {
      name = "Adwaita";
      package = pkgs.gnome3.gnome-themes-extra;
    };
  };

  programs.dircolors = {
    enable = true;
    settings = {
      DIR = "01;34;46";
    };
  };

  home.packages =
    let
      developmentPackages = with pkgs; [
        autoconf
        automake
        gcc
        hugo
        lsyncd
        pkg-config
      ];
      desktopPackages = with pkgs; [
        imhex
        brightnessctl
        qt5.qtwayland # QT_QPA_PLATFORM=wayland in home.sessionVariables
        isync
        looking-glass-client
        protonvpn-cli
        geeqie
        appimage-run
        v4l-utils
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
        graphviz
        openscad-unstable
        onedrive
        vscode
        # pkgs.bleeding.idea.idea-community
        aspell
        aspellDicts.ru
        aspellDicts.en
        aspellDicts.nl
        chromium
        desktop-file-utils
        # dunst
        evince
        freecad
        imagemagickBig
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
        skypeforlinux
        # workrave
        xdg-user-dirs
        xdotool
        flacon
        xlsfonts
        xorg.xdpyinfo
        xorg.xev
      ];
    in
       developmentPackages
    ++ lib.optionals config.hostConfig.feature.gui desktopPackages;
}
