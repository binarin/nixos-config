{lib, pkgs, config, system, ...}:

{
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "x-scheme-handler/https" = "smart-browser-chooser.desktop";
      "x-scheme-handler/http" = "smart-browser-chooser.desktop";
      "x-scheme-handler/org-protocol" = "org-protocol.desktop";
    };
    associations.added = {
      "application/pdf" = "org.gnome.Evince.desktop";
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 3600;
    maxCacheTtl = 14400;
    extraConfig = ''
      allow-preset-passphrase
    '';
    pinentryFlavor = "gtk2";
  };

  home.packages = with pkgs; [ arduino ];

}
