{ self, inputs, ... }:
{
  flake.nixosModules.impure-nix-setup =
    { ... }:
    {
      key = "nixos-config.modules.nixos.impure-nix-setup";

      config = {
        nix.extraOptions = ''
          gc-keep-outputs = true
          gc-keep-derivations = true
        '';

        nix.registry.nixpkgs.flake = inputs.nixpkgs;
        nix.registry.nixpkgs-unstable.flake = inputs.nixpkgs-unstable;
        nix.registry.flake-parts.flake = inputs.flake-parts;
      };
    };
}
