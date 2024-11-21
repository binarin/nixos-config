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
        @import url("${flake.inputs.ff-ultima}/userChrome.css");
        @import url("${zenburnCss}") (-moz-bool-pref: "user.theme.dark.zenburn");
      '';
      userContent = ''
        @import url("${config.zenburn.cssVars.file}");
        @import url("${flake.inputs.ff-ultima}/userContent.css");

        @-moz-document url(chrome://browser/content/browser.xul), url(about:newtab), url(about:home) {
          @media not (-moz-bool-pref: "browser.newtabpage.activity-stream.newtabWallpapers.v2.enabled") {
            @media (-moz-bool-pref: "user.theme.dark.zenburn"){body{background-color: var(--zenburn_bg) !important;}}
          }
        }
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

        # XXX try managing whitelist with home-manager?
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
      };

      extraConfig =
        with lib;
        pipe [ firebuilderEnabledText ff-ultima-defaults ] [
          (concatStringsSep "\n")
          (filterExplicitSettings config.programs.firefox.profiles.default.settings)
          builtins.readFile
        ];
    };
  };
}
