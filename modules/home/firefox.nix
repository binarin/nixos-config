{flake, config, lib, pkgs, ...}:

let
  firebuilderModules = {
    "blockImplicitOutbound.js" = true;
    "disableAutoUpdate.js" = true;
    "disableCaptivePortal.js" = true;
    "disableContentBlockingReports.js" = true;
    "disableCrashReports.js" = true;
    "disableNewtab.js" = true;
    "disablePasswords.js" = false;
    "disableSafeBrowsing.js" = false;
    "disableStudies.js" = true;
    "disableSync.js" = false;
    "disableTelemetry.js" = true;
    "dontWarnOnQuit.js" = false;
    "enableCustomization.js" = false; # already done below
    "geolocation.js" = true;
    "nicerfirefox.js" = true;
    "peskyfox.js" = true;
    "quieterfox.js" = true;
    "removeSponsored.js" = true;
    "requireSecure.js" = true;
  };
  firebuilderEnabledText =
    with lib;
    pipe firebuilderModules [
      (filterAttrs (name: enabled: enabled))
      attrNames
      (map (name: builtins.readFile "${flake.inputs.firebuilder}/modules/userjs/${name}"))
      (concatStringsSep "\n")
    ];

  ff-ultima-defaults = builtins.readFile "${flake.inputs.ff-ultima}/user.js";

  zenburnCss =
    with lib;
    pipe "firefox-theme-zenburn.css" [
      pkgs.flakeReadFile
      (replaceStrings ["@@ff-ultima@@"] ["${flake.inputs.ff-ultima}"])
      (pkgs.writeText "firefox-theme-zenburn.css")
    ];

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

  # XXX pull zenburn colors into home-manager
  wallpaper = pkgs.runCommand "image.png" {} ''
    ${pkgs.imagemagick}/bin/magick -size 1920x1080 "xc:#383838" $out
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
        @import url("${pkgs.flakeFile "zenburn-colors.css"}");
        @import url("${flake.inputs.ff-ultima}/userChrome.css");
        @import url("${zenburnCss}") (-moz-bool-pref: "user.theme.dark.zenburn");
      '';
      userContent = ''
        @import url("${pkgs.flakeFile "zenburn-colors.css"}");
        @import url("${flake.inputs.ff-ultima}/userContent.css");

        @-moz-document url(chrome://browser/content/browser.xul), url(about:newtab), url(about:home) {
          @media not (-moz-bool-pref: "browser.newtabpage.activity-stream.newtabWallpapers.v2.enabled") {
            @media (-moz-bool-pref: "user.theme.dark.zenburn"){body{background-image: url("${wallpaper}") !important;}}
          }
        }
      '';
      settings = {
        "font.minimum-size.x-western" = 16;
        "font.size.monospace.x-western" = 16;
        "intl.accept_languages" = "en,nl,ru";
        "media.videocontrols.picture-in-picture.enable-when-switching-tabs.enabled" = true;
        "browser.tabs.closeWindowWithLastTab" = false;
        "browser.sessionstore.restore_on_demand" = false;
        # "browser.sessionstore.restore_pinned_tabs_on_demand" = false;

        # XXX
        "media.autoplay.blocking_policy" = 2;

        "user.theme.dark.zenburn" = true;
        "user.theme.dark.a" = false;
        "user.theme.light.a" = false;
        "extensions.activeThemeID" = "default-theme@mozilla.org";

        "browser.aboutConfig.showWarning" = false;
        "browser.legacyUserProfileCustomizations.stylesheets" = true;
        "devtools.chrome.enabled" = true;
        "devtools.debugger.prompt-connection" = false;
        "devtools.debugger.remote-enabled" = true;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

        "gfx.webrender.all" = true;
        "layers.acceleration.force-enabled" = true;

        "browser.tabs.hoverPreview.enabled" = true;
        "sidebar.revamp" = true;
        "sidebar.verticalTabs" = true;
        "svg.context-properties.content.enabled" = true;
        "toolkit.tabbox.switchByScrolling" = false;
        "ultima.tabs.autohide" = false;
        "ultima.urlbar.centered" = false;
        "ultima.disable.windowcontrols.button" = true;

        # "browser.uiCustomization.state" = builtins.toJSON {
        #   currentVersion = 20;
        #   dirtyAreaCache = [ "nav-bar" "PersonalToolbar" "TabsToolbar" "unified-extensions-area" "vertical-tabs" ];
        #   newElementCount = 6;
        #   placements = {
        #     PersonalToolbar = [ "personal-bookmarks" ];
        #     TabsToolbar = [ ];
        #     nav-bar = [ "back-button" "forward-button" "stop-reload-button" "new-tab-button" "customizableui-special-spring1" "urlbar-container" "customizableui-special-spring2" "screenshot-button" "ublock0_raymondhill_net-browser-action" "_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action" "_8f8c4c52-216c-4c6f-aae0-c214a870d9d9_-browser-action" "_ddefd400-12ea-4264-8166-481f23abaa87_-browser-action" "_08f0f80f-2b26-4809-9267-287a5bdda2da_-browser-action" "_testpilot-containers-browser-action" "downloads-button" "sidebar-button" "unified-extensions-button" "alltabs-button" ];
        #     toolbar-menubar = [ "menubar-items" ];
        #     unified-extensions-area = [ "sponsorblocker_ajay_app-browser-action" "myallychou_gmail_com-browser-action" "savepage-we_dw-dev-browser-action" "_contain-facebook-browser-action" "enhancerforyoutube_maximerf_addons_mozilla_org-browser-action" "_12cf650b-1822-40aa-bff0-996df6948878_-browser-action" "_c49b13b1-5dee-4345-925e-0c793377e3fa_-browser-action" ];
        #     vertical-tabs = [ "tabbrowser-tabs" ];
        #     widget-overflow-fixed-list = [ ];
        #   };
        #   seen = [ "save-to-pocket-button" "developer-button" "ublock0_raymondhill_net-browser-action" "myallychou_gmail_com-browser-action" "savepage-we_dw-dev-browser-action" "_08f0f80f-2b26-4809-9267-287a5bdda2da_-browser-action" "_8f8c4c52-216c-4c6f-aae0-c214a870d9d9_-browser-action" "_ddefd400-12ea-4264-8166-481f23abaa87_-browser-action" "_contain-facebook-browser-action" "_testpilot-containers-browser-action" "enhancerforyoutube_maximerf_addons_mozilla_org-browser-action" "_12cf650b-1822-40aa-bff0-996df6948878_-browser-action" "_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action" "_c49b13b1-5dee-4345-925e-0c793377e3fa_-browser-action" "sponsorblocker_ajay_app-browser-action" ];
        # };
      };

      extraConfig =
        with lib;
        pipe [ firebuilderEnabledText ff-ultima-defaults ] [
          (concatStringsSep "\n")
          (filterExplicitSettings config.programs.firefox.profiles.default.settings)
          (der: builtins.trace "${der}" der)
          builtins.readFile
        ];
    };
  };
}
