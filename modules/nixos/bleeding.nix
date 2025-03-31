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
  nixpkgs.overlays = lib.mkIf (config.hostConfig.feature.bleeding) [
    (final: prev: {
      bleeding = import inputs.nixpkgs-unstable {
        inherit (prev) system;
        config = config.nixpkgs.config;
        overlays = [
          (bf: bp: {
            # diverges too fast, leads to segfaults. a lot of rebuilds is the price.
            mesa = final.mesa.override {
              libdrm = final.bleeding.libdrm;
            };
          })
        ];
      };
      bleeding-staging = import inputs.nixpkgs-staging {
        inherit (prev) system;
        config = config.nixpkgs.config;
        overlays = [
          (bf: bp: {
            # diverges too fast, leads to segfaults. a lot of rebuilds is the price.
            mesa = final.mesa.override {
              libdrm = final.bleeding.libdrm;
            };
          })
        ];
      };
    })
  ];
}
