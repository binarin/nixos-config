{ flake, pkgs, lib, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  config = lib.mkIf pkgs.stdenv.isLinux {
    nixpkgs.overlays = [ self.overlays.lnxlink ];
  };
}
