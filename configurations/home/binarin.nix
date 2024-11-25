{
  flake,
  pkgs,
  lib,
  system,
  config,
  nixosConfig,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [ self.homeModules.default ];
}
