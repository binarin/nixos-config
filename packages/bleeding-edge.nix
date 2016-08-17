{config, pkgs, lib, ...}:

let
  master-nixpkgs-checkout = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    name = "nixpkgs-master-2016-08-16";
    rev = "1759825b34cad43f8925aa015c80f56a57eb12e6";
    sha256 = "1i0hh8jnfhs1v0ziwkga34zr6p0dlznsh9pa3kxz34wz8hf7vnp2";
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
