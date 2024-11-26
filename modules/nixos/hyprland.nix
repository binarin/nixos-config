# -*- nix -*-
{
  flake,
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{

  # disabledModules = [
  #   "programs/wayland/hyprland.nix"
  # ];

  # imports = [
  #   "${self.inputs.nixpkgs-unstable}/nixos/modules/programs/wayland/hyprland.nix"
  #   "${self.inputs.nixpkgs-unstable}/nixos/modules/programs/wayland/uwsm.nix"
  #   "${self.inputs.nixpkgs-unstable}/nixos/modules/services/misc/graphical-desktop.nix"
  # ];

  # options.services.speechd = lib.mkOption { type = lib.types.attrsOf lib.types.anything; };

  config = lib.mkIf config.hostConfig.feature.hyprland {
    nixpkgs.overlays = [
      inputs.hyprland-contrib.overlays.default
      inputs.hyprland.overlays.default # NOTE: goes last! previous overlays can mess with the same overrides
    ];

    services.displayManager.sddm.enable = true;
    services.displayManager.defaultSession = "hyprland-uwsm";

    programs.hyprlock.enable = true;
    programs.hyprland.enable = true;
    programs.hyprland.withUWSM = true;
  };
}
