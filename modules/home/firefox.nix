{flake, config, lib, pkgs, ...}:

let
  filterExplicitSettings = settings: text:
    with lib;
    let
      regexpAlternatives = pipe settings [
        attrNames
        (map (nm: ''\Q${nm}\E''))
        (concatStringsSep "|")
      ];
      regexp = ''"(?:${regexpAlternatives})"'';
    in
      pkgs.runCommand "filtered-user.js" {} ''
        cat <<'EOF' > $out
        // ${regexp}
        EOF
        cat <<'EOF' | ${lib.getExe pkgs.perl} -pE 'print "// NIX-DISABLED:" if /${regexp}/' >> $out
        ${text}
        EOF
      '';
in
{
  xdg.mimeApps.defaultApplications = {
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
  };

  programs.firefox = {
    enable = true;
    profiles.default = {
      id = 0;
      isDefault = true;
      userChrome = ''
        @import url("${config.zenburn.cssVars.file}");
        @import url("${config.lib.self.file "firefox-theme-zenburn.css"}");

        /* hide min/max/close buttons */
        #titlebar {
          display: none !important;
        }
      '';

      userContent = ''
      '';

      settings = {
        "font.minimum-size.x-western" = 16;
        "font.size.monospace.x-western" = 16;
        "intl.accept_languages" = "en,nl,ru";
        "media.videocontrols.picture-in-picture.enable-when-switching-tabs.enabled" = true;
        "browser.tabs.closeWindowWithLastTab" = false;

        # restore last session
        "browser.startup.page" = 3;
        "browser.sessionstore.restore_on_demand" = true;
        "browser.sessionstore.restore_pinned_tabs_on_demand" = false;

        "media.autoplay.blocking_policy" = 2;

        "extensions.activeThemeID" = "default-theme@mozilla.org";

        "browser.aboutConfig.showWarning" = false;
        "browser.legacyUserProfileCustomizations.stylesheets" = true;
        "devtools.chrome.enabled" = true;
        "devtools.debugger.prompt-connection" = false;
        "devtools.debugger.remote-enabled" = true;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

        "gfx.webrender.all" = lib.mkIf (!config.hostConfig.feature.wsl) true;
        "layers.acceleration.force-enabled" = lib.mkIf (!config.hostConfig.feature.wsl) true;

        "browser.tabs.hoverPreview.enabled" = true;
        "sidebar.revamp" = true;
        "sidebar.verticalTabs" = true;
        "svg.context-properties.content.enabled" = true;
        "toolkit.tabbox.switchByScrolling" = false;

        # no reason to format it, it's copied from the browser every time
        "browser.uiCustomization.state" = ''{"placements":{"widget-overflow-fixed-list":[],"unified-extensions-area":["sponsorblocker_ajay_app-browser-action","ublock0_raymondhill_net-browser-action","myallychou_gmail_com-browser-action","_contain-facebook-browser-action","addon_darkreader_org-browser-action","enhancerforyoutube_maximerf_addons_mozilla_org-browser-action","_12cf650b-1822-40aa-bff0-996df6948878_-browser-action","_c49b13b1-5dee-4345-925e-0c793377e3fa_-browser-action"],"nav-bar":["back-button","forward-button","stop-reload-button","customizableui-special-spring1","urlbar-container","alltabs-button","new-tab-button","customizableui-special-spring2","unified-extensions-button","_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action","_ddefd400-12ea-4264-8166-481f23abaa87_-browser-action","_8f8c4c52-216c-4c6f-aae0-c214a870d9d9_-browser-action","_08f0f80f-2b26-4809-9267-287a5bdda2da_-browser-action","savepage-we_dw-dev-browser-action","_testpilot-containers-browser-action","downloads-button","sidebar-button"],"toolbar-menubar":["menubar-items"],"TabsToolbar":[],"vertical-tabs":["tabbrowser-tabs"],"PersonalToolbar":["personal-bookmarks"]},"seen":["save-to-pocket-button","developer-button","ublock0_raymondhill_net-browser-action","myallychou_gmail_com-browser-action","savepage-we_dw-dev-browser-action","_08f0f80f-2b26-4809-9267-287a5bdda2da_-browser-action","_8f8c4c52-216c-4c6f-aae0-c214a870d9d9_-browser-action","_ddefd400-12ea-4264-8166-481f23abaa87_-browser-action","_contain-facebook-browser-action","_testpilot-containers-browser-action","addon_darkreader_org-browser-action","enhancerforyoutube_maximerf_addons_mozilla_org-browser-action","_12cf650b-1822-40aa-bff0-996df6948878_-browser-action","_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action","_c49b13b1-5dee-4345-925e-0c793377e3fa_-browser-action","sponsorblocker_ajay_app-browser-action"],"dirtyAreaCache":["nav-bar","PersonalToolbar","TabsToolbar","unified-extensions-area","vertical-tabs"],"currentVersion":20,"newElementCount":3}'';
      };

      extraConfig = ''
      '' + (
        with lib;
        pipe [ (builtins.readFile "${flake.inputs.user-js}/user.js") ] [
          (concatStringsSep "\n")
          (filterExplicitSettings config.programs.firefox.profiles.default.settings)
          builtins.readFile
        ]
      );

    };
    profiles.test = {
      id = 1;
    };
  };
}
