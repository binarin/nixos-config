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
  config = {
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
          "firefox"
          "fzf"
          "gtk"
          "swaync"
          "vscode"
          "fuzzel"
          "zellij"
        ]
        (nm: {
          enable = true;
        });
  };
}
