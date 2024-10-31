{ flake, lib, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    self.nixosModules.default
    ./configuration.nix
  ];

  nixpkgs.hostPlatform = lib.mkForce "x86_64-linux";

  hostOptions.gui.enable = true;
  hostOptions.managedUsers = [ "binarin" ];
}
