# See /modules/nixos/* for actual settings
# This file is just *top-level* configuration.
{ flake, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  networking.hostName = "furfur";

  imports = [
    self.nixosModules.default
    self.nixosModules.emacs
    ./configuration.nix
  ];

  hostOptions.gui.enable = true;
  hostOptions.managedUsers = [ "binarin" ];
}
