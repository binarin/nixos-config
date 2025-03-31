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
      ( final: prev: {
        libgbm = prev.mesa;
        hyprland = prev.hyprland.override {
          wayland-protocols = final.bleeding.wayland-protocols;
        };
        hypridle = prev.hypridle.override {
          stdenv = prev.gcc14Stdenv;
        };
        hyprlock = prev.hyprlock.override {
          stdenv = prev.gcc14Stdenv;
        };
        hyprpicker = final.bleeding.hyprpicker;
        xdg-desktop-portal-hyprland = prev.xdg-desktop-portal-hyprland.override {
          stdenv = prev.gcc14Stdenv;
        };
        uwsm = prev.uwsm.overrideAttrs {
          version = "0.20.5-dev-5fafff9";
          src = pkgs.fetchFromGitHub {
            owner = "Vladimir-csp";
            repo = "uwsm";
            rev = "5fafff9a1e0464b46a528193c07f36a791d912b6";
            hash = "sha256-yjKKDizL0Ibne1oUn18h21XBfpGYdMFyD/wdSPQA/Zs=";
          };
        };
      } )
    ];

    services.displayManager.sddm.enable = true;
    services.displayManager.sddm.wayland.enable = true;
    services.displayManager.defaultSession = "hyprland-uwsm";

    security.pam.services.login.kwallet.enable = true;
    security.pam.services.login.kwallet.package = lib.mkForce pkgs.kdePackages.kwallet-pam;

    programs.hyprlock.enable = true;
    programs.hyprland.enable = true;
    programs.hyprland.withUWSM = true;
  };
}
