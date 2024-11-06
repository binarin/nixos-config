{ flake, lib, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  networking.hostName = "valak";

  imports = [
    self.nixosModules.default
    ./configuration.nix
  ];

  hostOptions.gui.enable = true;
  hostOptions.managedUsers = [ "binarin" ];
}
