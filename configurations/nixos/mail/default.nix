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

  networking.hostName = "mail";
  hostConfig.features = [
    "lxc"
    "tailscale"
  ];

  system.stateVersion = "24.05";
}
