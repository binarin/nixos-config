{config, pkgs, lib, ...}:

let
  master-nixpkgs-checkout = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    name = "nixpkgs-master-2017-02-12";
    rev = "31eba21d1dab7da2e75be229c542990e8f4b56b6";
    sha256 = "1mqx7c5jwgnka8zb83hq7wb955qy4jb2mk44b3wvan8w1a0sqc88";
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
