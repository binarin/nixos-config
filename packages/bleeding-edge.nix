{config, pkgs, lib, ...}:

let
  master-nixpkgs-checkout = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    name = "nixpkgs-master-2017-05-11";
    rev = "8b17444047eef7c01821dbb5a822d81708c2638a";
    sha256 = "0m734942g4hw675f51m3wx6b0g8s5xrhkqn750q7yzagfp9z7kvk";
  };
 # master-nixpkgs-checkout = /home/binarin/personal-workspace/nixpkgs;
 master-nixpkgs = import master-nixpkgs-checkout {inherit (config.nixpkgs) config;};
in
with lib;
{
  options = {
    bleeding = {
      pkgs = mkOption {
        type = types.attrs;
      };
    };
  };
  config = {
    bleeding = {
      pkgs = master-nixpkgs;
    };
  };
}
