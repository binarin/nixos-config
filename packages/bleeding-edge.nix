{config, pkgs, lib, ...}:

let
  master-nixpkgs-checkout = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    name = "nixpkgs-master-2016-10-19";
    rev = "306a8d9e8ca90d7a1fac141cca35dfe19750f9cd";
    sha256 = "0kmxznhx72ir65xy85zy661870xbi71a1hzvc6s6334lx1vrkkss";
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
