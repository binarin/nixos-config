{config, pkgs, lib, ...}:

let
  master-nixpkgs-checkout = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    name = "nixpkgs-master-2016-11-23";
    rev = "7a038b931158ae6d1605d6456709b9a9fa90a3a6";
    sha256 = "11aicmbw96i99h207bqxrggxp1anzfhrsp6a4nyxfgxki94jl0d2";
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
