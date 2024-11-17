{ config, pkgs, lib, ... }:
let
  ignoringVulns = x: x // { meta = (x.meta // { knownVulnerabilities = [ ]; }); };
  qtwebkitIgnoringVulns = pkgs.qt5.qtwebkit.overrideAttrs ignoringVulns;

  guiPackages = with pkgs; [
    tdesktop
    thunderbird
    vlc
    chromium
    google-chrome

    # anki-bin
    # discord
    # electrum
    # gphoto2
    # jetbrains.idea-community
    # kdenlive
    # picard
    # remmina
    # signal-desktop
    # wdisplays
    # winePackages.full
  ];

  slowRebuildGuiPackages = with pkgs; [
    (goldendict.override { qtwebkit = qtwebkitIgnoringVulns; })
  ];

  inherit (lib) optionals;
  inherit (config.hostConfig) feature;
in

{
  home.packages = optionals feature.gui (builtins.concatLists [
    guiPackages
    (optionals (!feature.fast-rebuild) slowRebuildGuiPackages)
  ]);
}
