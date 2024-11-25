{
  flake,
  lib,
  pkgs,
  config,
  ...
}:
{
  config = lib.mkIf config.hostConfig.feature.gui {
    programs.firefox = {
      enable = true;
      policies = {
        Permissions = {
          Autoplay = {
            Allow = [
              "https://jellyfin.binarin.info"
              "https://ts.binarin.info"
            ];
          };
        };
      };
      languagePacks = [
        "en-US"
        "en-GB"
        "nl"
        "ru"
        "es-ES"
      ];
    };
  };
}
