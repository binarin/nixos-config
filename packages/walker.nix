{ flake, ...}:
{ pkgs, fetchFromGitHub, ... }:

pkgs.callPackage "${flake.inputs.walker}/nix/package.nix" {}
