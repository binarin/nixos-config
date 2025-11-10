{ flake, ... }:
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

  networking.hostName = "docker-on-nixos";

  impermanence.enable = true;

  hostConfig.features = [
    "lxc"
    "tailscale"
  ];

  system.stateVersion = "24.11";
}
