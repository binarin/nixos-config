# See /modules/nixos/* for actual settings
# This file is just *top-level* configuration.
{ flake, lib, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  networking.hostName = "forgejo";
  imports = [
    self.nixosModules.default
    ./configuration.nix
  ];
}
