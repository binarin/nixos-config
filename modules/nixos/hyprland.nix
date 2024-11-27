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
  config = lib.mkIf config.hostConfig.feature.hyprland {
    nixpkgs.overlays = [
      inputs.hyprland-contrib.overlays.default
      inputs.hyprland.overlays.default
    ];

    services.displayManager.sddm.enable = true;
    services.displayManager.defaultSession = "hyprland-uwsm";

    security.pam.services.login.kwallet.enable = true;
    security.pam.services.login.kwallet.package = pkgs.kdePackages.kwallet-pam;

    programs.hyprlock.enable = true;
    programs.hyprland.enable = true;
    programs.hyprland.withUWSM = true;
  };
}
