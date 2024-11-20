{flake, lib, config, pkgs, ...}:
{

  stylix.targets = lib.genAttrs ["bat" "tmux" "btop" "firefox" "fzf" "gtk" "hyprland" "swaync" "vscode" "fuzzel"] (nm: {
    enable = true;
  });
}
