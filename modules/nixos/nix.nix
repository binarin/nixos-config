{ flake, pkgs, lib, config, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  config = {
    nix = {
      settings = {
        sandbox = true;
        substituters = [ "https://cache.nixos.org" ];
      };
      extraOptions = ''
        experimental-features = nix-command flakes ca-derivations
      '';
    };
    nix.settings.trusted-users = [ "root" ];

    nixpkgs.config = {
      allowUnfree = true;
      oraclejdk.accept_license = true;
    };
  };
}
