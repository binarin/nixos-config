{
  flake,
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [
    flake.inputs.stylix.homeModules.stylix
  ];
  config = lib.mkMerge [
    {
      # XXX Another copy in nixos/stylix.nix
      stylix.cursor = {
        package = pkgs.bibata-cursors;
        name = "Bibata-Modern-Amber";
        size = 16;
      };

      stylix.targets =
        lib.genAttrs
          [
            "bat"
            "tmux"
            "btop"
            "fzf"
            "zellij"
          ]
          (nm: {
            enable = true;
          });
    }
    (lib.mkIf config.hostConfig.feature.gui {
      stylix.targets = lib.genAttrs [
        "swaync"
        "vscode"
        "fuzzel"
        "firefox"
        "gtk"
      ] (nm: {
        enable = true;
      });
    })
  ];
  }
