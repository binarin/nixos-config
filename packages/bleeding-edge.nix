{config, pkgs, lib, ...}:

let
  master-nixpkgs-checkout = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    name = "nixpkgs-master-2017-03-20";
    rev = "20194e2696a276937bfe7b448951036831c69700";
    sha256 = "1a2g3xkcms7jbp42a57nsyrhd9x5l14y4ycpars7b0sir5896jx9";
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
