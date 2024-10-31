# -*- nix -*-
{ flake, config, pkgs, lib, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  nixpkgs.overlays = [
    (final: prev: {
      bleeding = import inputs.nixpkgs-unstable
        {
          inherit (prev) system;
          config = config.nixpkgs.config;
          overlays = [
            (bf: bp: { mesa = final.mesa; }) # diverges too fast, leads to segfaults. a lot of rebuilds is the price.
          ];
        };
    })
  ];
}
