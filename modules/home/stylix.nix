{flake, lib, config, pkgs, ...}:
{
  imports = [
    flake.inputs.stylix.homeManagerModules.stylix
  ];
  config = {
    stylix.targets = lib.genAttrs ["bat" "tmux" "btop" "firefox" "fzf" "gtk" "hyprland" "swaync" "vscode" "fuzzel"] (nm: {
      enable = true;
    });
  };
}
