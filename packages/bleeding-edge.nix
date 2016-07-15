{config, pkgs, lib, ...}:

let
  # master-nixpkgs-checkout = pkgs.fetchFromGitHub {
  #   owner = "NixOS";
  #   repo = "nixpkgs";
  #   rev = "11a2edc2ea17411699ccf5196b84898dd2d91302";
  #   sha256 = "1sjdmbz8mq0k3z9x3h6h33vr082ax0dbhkwk9m5m4qdz5qd9wjm6";
  # };
  master-nixpkgs-checkout = "/home/binarin/personal-workspace/nixpkgs/";
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
