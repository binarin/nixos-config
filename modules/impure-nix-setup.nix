{ self, inputs, ... }:
{
  flake.nixosModules.impure-nix-setup =
    { ... }:
    {
      key = "nixos-config.modules.nixos.impure-nix-setup";

      imports = [
        self.modules.generic.flake-files
      ];

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
