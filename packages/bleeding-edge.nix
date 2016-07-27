{config, pkgs, lib, ...}:

let
  master-nixpkgs-checkout = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    name = "nixpkgs-master-2016-07-27";
    rev = "381967327ff7e9885a6ebd32b13580a8a95ee5fc";
    sha256 = "08n6lg9vni9d24nbg9vd77cv6yk817078z5839i6vfbdqm5r3as3";
  };
#  master-nixpkgs-checkout = /home/binarin/personal-workspace/nixpkgs;
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
