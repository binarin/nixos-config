{flake, lib, pkgs, config, ...}:
{
  config = lib.mkIf config.hostConfig.feature.hyprland {
    nixpkgs.overlays = [
      # flake.inputs.waybar.overlays.default
    ];
  };
}
