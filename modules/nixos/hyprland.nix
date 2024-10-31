# -*- nix -*-
{ flake, config, pkgs, lib, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  nixpkgs.overlays = [
    inputs.hyprland.overlays.default
    inputs.hyprland-contrib.overlays.default

    (final: prev: {
      aquamarine = prev.aquamarine.override { libinput = final.bleeding.libinput; };
      xdg-desktop-portal-hyprland = prev.xdg-desktop-portal-hyprland.override {
        pipewire = final.bleeding.pipewire;
      };
      hyprland = prev.hyprland.override {
        wayland = final.bleeding.wayland;
        wayland-scanner = final.bleeding.wayland-scanner;
        libinput = final.bleeding.libinput;
      };
    })
  ];

  services.displayManager.sddm.enable = true;
  services.displayManager.defaultSession = "hyprland";

  programs.hyprlock.enable = true;
  programs.hyprland.enable = true;
}
