{ flake, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  environment.etc."nix/inputs/nixpkgs".source = "${inputs.nixpkgs}";
  environment.etc."nix/inputs/self-absolute-path".text = "${self}";
  environment.etc."nix/overlays/self-overlays.nix".source = ./self-overlays.nix;
  environment.etc."nix/nixpkgs-config.nix".source = ./nixpkgs-config.nix;

  environment.variables.NIXPKGS_CONFIG = "/etc/nix/nixpkgs-config.nix";

  nix.nixPath = [
    "nixpkgs=/etc/nix/inputs/nixpkgs"
    "nixpkgs-overlays=/etc/nix/overlays"
  ];

  nix.registry.nixpkgs.flake = flake.inputs.nixpkgs;
}
