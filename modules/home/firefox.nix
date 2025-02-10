{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.hostConfig.feature.gui {

    # XXX can't link because home-manager home.files doesn't allow creating files outside of $HOME
    # And with symlink it's detectable, in comparison to a bind mount
    impermanence.local-bind-directories-no-root = [ ".mozilla/firefox" ];

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
      profiles.default = {
        id = 0;
        isDefault = true;

        userChrome =
          (config.lib.self.read "firefox-userChrome.css");

        userContent = ''
        '';

        settings = {
          "browser.aboutConfig.showWarning" = false;
          "browser.legacyUserProfileCustomizations.stylesheets" = true;
          "browser.search.suggest.enabled" = true;
          "browser.sessionstore.restore_on_demand" = true;
          "browser.sessionstore.restore_pinned_tabs_on_demand" = false;
          "browser.startup.page" = 3;
          "browser.tabs.closeWindowWithLastTab" = false;
          "browser.tabs.hoverPreview.enabled" = true;
          "browser.uiCustomization.state" = ''{"placements":{"widget-overflow-fixed-list":[],"unified-extensions-area":["sponsorblocker_ajay_app-browser-action","ublock0_raymondhill_net-browser-action","myallychou_gmail_com-browser-action","_contain-facebook-browser-action","addon_darkreader_org-browser-action","enhancerforyoutube_maximerf_addons_mozilla_org-browser-action","_12cf650b-1822-40aa-bff0-996df6948878_-browser-action","_c49b13b1-5dee-4345-925e-0c793377e3fa_-browser-action"],"nav-bar":["back-button","forward-button","stop-reload-button","customizableui-special-spring1","urlbar-container","alltabs-button","new-tab-button","customizableui-special-spring2","unified-extensions-button","_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action","_ddefd400-12ea-4264-8166-481f23abaa87_-browser-action","_8f8c4c52-216c-4c6f-aae0-c214a870d9d9_-browser-action","_08f0f80f-2b26-4809-9267-287a5bdda2da_-browser-action","savepage-we_dw-dev-browser-action","_testpilot-containers-browser-action","downloads-button","sidebar-button"],"toolbar-menubar":["menubar-items"],"TabsToolbar":[],"vertical-tabs":["tabbrowser-tabs"],"PersonalToolbar":["personal-bookmarks"]},"seen":["save-to-pocket-button","developer-button","ublock0_raymondhill_net-browser-action","myallychou_gmail_com-browser-action","savepage-we_dw-dev-browser-action","_08f0f80f-2b26-4809-9267-287a5bdda2da_-browser-action","_8f8c4c52-216c-4c6f-aae0-c214a870d9d9_-browser-action","_ddefd400-12ea-4264-8166-481f23abaa87_-browser-action","_contain-facebook-browser-action","_testpilot-containers-browser-action","addon_darkreader_org-browser-action","enhancerforyoutube_maximerf_addons_mozilla_org-browser-action","_12cf650b-1822-40aa-bff0-996df6948878_-browser-action","_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action","_c49b13b1-5dee-4345-925e-0c793377e3fa_-browser-action","sponsorblocker_ajay_app-browser-action"],"dirtyAreaCache":["nav-bar","PersonalToolbar","TabsToolbar","unified-extensions-area","vertical-tabs"],"currentVersion":20,"newElementCount":3}'';
          "browser.urlbar.suggest.searches" = true;
          "devtools.chrome.enabled" = true;
          "devtools.debugger.prompt-connection" = false;
          "devtools.debugger.remote-enabled" = true;
          "extensions.activeThemeID" = "default-theme@mozilla.org";
          "font.minimum-size.x-western" = 16;
          "font.size.monospace.x-western" = 16;
          "gfx.webrender.all" = !config.hostConfig.feature.wsl;
          "intl.accept_languages" = "en,nl,ru";
          "layers.acceleration.force-enabled" = !config.hostConfig.feature.wsl;
          "media.autoplay.blocking_policy" = 2;
          "media.videocontrols.picture-in-picture.enable-when-switching-tabs.enabled" = true;
          "privacy.clearOnShutdown_v2.cookiesAndStorage" = false;
          "privacy.clearOnShutdown_v2.historyFormDataAndDownloads" = false;
          "privacy.sanitize.sanitizeOnShutdown" = false;
          "sidebar.revamp" = true;
          "sidebar.verticalTabs" = config.hostConfig.feature.airgapped; # no sideberry extension on airgapped machines
          "svg.context-properties.content.enabled" = true;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "toolkit.tabbox.switchByScrolling" = false;
          "ui.key.chromeAccess" = 3; # Ctrl-Shift
          "ui.key.contentAccess" = 5; # Alt-Shift
          "ui.key.menuAccessKey" = -1;
        };
      };
      profiles.test = {
        id = 1;
      };
    };
  };
}
