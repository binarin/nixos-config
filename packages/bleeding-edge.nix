{config, pkgs, lib, ...}:

let
  master-nixpkgs-checkout = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    name = "nixpkgs-master-2017-01-26";
    rev = "a1af9cc1cf04f4cc4080d16850fbe82bc8933ec7";
    sha256 = "1rjhniv8l79yp0gij43pw1i3rid2a8f1g3ls0q5jyyakpry8km01";
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
