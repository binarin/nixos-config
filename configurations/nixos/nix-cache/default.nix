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
    ./configuration.nix
  ];

  networking.hostName = "nix-cache";

  hostConfig.features = [
    "lxc"
    "nix-builder"
    "interactive-cli"
  ];

  system.stateVersion = "24.11";
}
