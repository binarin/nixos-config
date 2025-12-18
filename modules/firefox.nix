{
  ...
}:
{
  flake.nixosModules.firefox =
    {
      ...
    }:
    {
      key = "nixos-config.modules.nixos.firefox";

      config = {
        programs.firefox = {
          enable = true;
          policies = {
            Permissions = {
              Autoplay = {
                Allow = [
                  "https://jellyfin.binarin.info"
                  "https://ts.binarin.info"
                  "https://navidrome.binarin.info"
                  "https://youtube.com"
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
    };

  flake.homeModules.firefox =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.home.firefox";

      config = {
        stylix.targets.firefox.enable = true;
        stylix.targets.firefox.profileNames = [ "clean" ];
        stylix.targets.firefox.firefoxGnomeTheme.enable = true;

        impermanence.local-directories = [ ".mozilla/firefox" ];

        home.packages = [
          (pkgs.writeShellScriptBin "x-www-browser" ''
            exec firefox "$@"
          '')
        ];

        xdg.mimeApps.defaultApplications = {
          "x-scheme-handler/http" = "firefox.desktop";
          "x-scheme-handler/https" = "firefox.desktop";
        };

        programs.firefox = {
          enable = true;
          profiles.clean = {
            id = 0;
            isDefault = true;

            userChrome = (config.lib.self.read "firefox-userChrome.css");

            userContent = '''';

            settings = {
              "browser.aboutConfig.showWarning" = false;
              "browser.legacyUserProfileCustomizations.stylesheets" = true;
              "browser.search.suggest.enabled" = true;
              "browser.sessionstore.restore_on_demand" = true;
              "browser.sessionstore.restore_pinned_tabs_on_demand" = false;
              "browser.startup.page" = 3;
              "browser.tabs.closeWindowWithLastTab" = false;
              "browser.tabs.hoverPreview.enabled" = true;
              "browser.urlbar.suggest.searches" = true;
              "devtools.chrome.enabled" = true;
              "devtools.debugger.prompt-connection" = false;
              "devtools.debugger.remote-enabled" = true;
              "extensions.activeThemeID" = "default-theme@mozilla.org";
              "ui.systemUsesDarkTheme" = true;
              "font.minimum-size.x-western" = lib.mkForce 16;
              "font.size.monospace.x-western" = lib.mkForce 16;
              "gfx.webrender.all" = true;
              "intl.accept_languages" = "en,nl,ru";
              "layers.acceleration.force-enabled" = true;
              "media.autoplay.blocking_policy" = 2;
              "media.videocontrols.picture-in-picture.enable-when-switching-tabs.enabled" = true;
              "privacy.clearOnShutdown_v2.cookiesAndStorage" = false;
              "privacy.clearOnShutdown_v2.historyFormDataAndDownloads" = false;
              "privacy.sanitize.sanitizeOnShutdown" = false;
              "sidebar.revamp" = true;
              "sidebar.verticalTabs" = false;
              "svg.context-properties.content.enabled" = true;
              "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
              "toolkit.tabbox.switchByScrolling" = false;
              "ui.key.chromeAccess" = 3; # Ctrl-Shift
              "ui.key.contentAccess" = 5; # Alt-Shift
              "ui.key.menuAccessKey" = -1;
            };
          };
        };
      };
    };
}
