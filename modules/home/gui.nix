{
  config,
  pkgs,
  lib,
  ...
}:
let
  ignoringVulns =
    x:
    x
    // {
      meta = x.meta // {
        knownVulnerabilities = [ ];
      };
    };
  qtwebkitIgnoringVulns = pkgs.qt5.qtwebkit.overrideAttrs ignoringVulns;

  texlive-combined = pkgs.texlive.combine { inherit (pkgs.texlive) scheme-full beamer ps2eps; };

  guiPackages = with pkgs; [
    protonmail-bridge
    appimage-run
    chromium
    evince
    flacon
    geeqie
    gimp
    gitg
    glxinfo
    imagemagickBig
    imhex
    libnotify
    mplayer
    insync
    protonvpn-cli
    skypeforlinux
    steam-run
    tdesktop
    texlive-combined
    thunderbird
    usbutils.python
    vlc
    vscode
    xdg-user-dirs
    xdotool
    xorg.xhost
  ];

  slowRebuildGuiPackages = with pkgs; [
    # (goldendict.override { qtwebkit = qtwebkitIgnoringVulns; })
  ];

  inherit (lib) optionals;
  inherit (config.hostConfig) feature;
in
{
  config = lib.mkIf feature.gui {
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/tg" = "org.telegram.desktop.desktop";
        "image/jpeg" = "geeqie.desktop";
      };
      associations.added = {
        "application/pdf" = "org.gnome.Evince.desktop";
        "image/jpeg" = "geeqie.desktop";
      };
    };
    xdg.configFile."mimeapps.list".force = true;
    gtk = {
      enable = true;
      # iconTheme = {
      #   package = pkgs.gnome3.adwaita-icon-theme;
      #   name = "Adwaita";
      # };
    };
    home.packages = guiPackages ++ (optionals (!feature.fast-rebuild) slowRebuildGuiPackages);
  };
}
