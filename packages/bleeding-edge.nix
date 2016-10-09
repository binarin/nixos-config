{config, pkgs, lib, ...}:

let
  master-nixpkgs-checkout = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    name = "nixpkgs-master-2016-10-04";
    rev = "94e87bd79bd04bc54efef21b93c8bbf5ffec40e8";
    sha256 = "1wimgbxrzadkjp5mk8ngcdbj2mljqhj69wk6b92ba8qqsc3p4ps7";
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
