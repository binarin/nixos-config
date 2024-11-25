{
  pkgs,
  flake,
  config,
  lib,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  config = lib.mkIf config.hostConfig.feature.nix-builder {
    environment.etc."nix/inputs/nixpkgs".source = "${inputs.nixpkgs}";
    environment.etc."nix/inputs/self-absolute-path".text = "${self}";
    environment.etc."nix/overlays/self-overlays.nix".source = config.lib.self.file "nixos-configuration-overlays-for-impure-nix-commands.nix";
    environment.etc."nix/nixpkgs-config.nix".source = config.lib.self.file "nixos-configuration-nix-config-for-impure-nix-commands.nix";

    environment.variables.NIXPKGS_CONFIG = "/etc/nix/nixpkgs-config.nix";

    nix.nixPath = [
      "nixpkgs=/etc/nix/inputs/nixpkgs"
      "nixpkgs-overlays=/etc/nix/overlays"
    ];

    nix.registry.nixpkgs.flake = flake.inputs.nixpkgs;
  };
}
