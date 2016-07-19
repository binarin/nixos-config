{config, pkgs, lib, ...}:

let
  master-nixpkgs-checkout = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "d45802973fa3281fdf10c6dafa9b6889b64b5b2f";
    sha256 = "1iwjsbkqzrf9knv7jc9k4hrk50lws8nh0zgbpgyrk8fg78sadfd6";
  };
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
