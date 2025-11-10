# See /modules/nixos/* for actual settings
# This file is just *top-level* configuration.
{
  flake,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    self.nixosModules.default
    flake.inputs.self.nixosModules.user-binarin
    inputs.arion.nixosModules.arion
    ./configuration.nix
  ];

  networking.hostName = "media";
  hostConfig.features = [
    "lxc"
    "interactive-cli"
  ];

}
