{ flake, config, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    self.nixosModules.default
    "${inputs.nixpkgs}/nixos/modules/profiles/minimal.nix"
    ./configuration.nix
  ];

  inventoryHostName = "docker-on-nixos";

  hostConfig.deployHostName = config.hostConfig.ipAllocation.home.primary.address;

  hostConfig.features = [
    "lxc"
    "impermanence"
    "tailscale"
  ];

  system.stateVersion = "24.11";
}
