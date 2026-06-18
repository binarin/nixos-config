{ self, ... }:
{
  flake.homeModules.desktop-essentials =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.desktop-essentials";

      home.packages = with pkgs; [
        brownnoise
        sox
        mplayer
        vlc
        geeqie
        gimp
        imagemagickBig
        evince
        pdftk
        bluetui
      ];

      xdg.mimeApps.defaultApplications = {
        "image/jpeg" = "geeqie.desktop";
        "application/pdf" = "org.gnome.Evince.desktop";
      };
      xdg.mimeApps.associations.added = {
        "application/pdf" = "org.gnome.Evince.desktop";
        "image/jpeg" = "geeqie.desktop";
      };
    };
}
