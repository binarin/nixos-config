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

        nix.registry.nixpkgs = {
          from = {
            type = "indirect";
            id = "nixpkgs";
          };
          to = {
            type = "path";
            path = inputs.nixpkgs.outPath;
          };
        };
        nix.registry.nixpkgs-unstable = {
          from = {
            type = "indirect";
            id = "nixpkgs-unstable";
          };
          to = {
            type = "path";
            path = inputs.nixpkgs-unstable.outPath;
          };
        };
        nix.registry.flake-parts = {
          from = {
            type = "indirect";
            id = "flake-parts";
          };
          to = {
            type = "path";
            path = inputs.flake-parts.outPath;
          };
        };
      };
    };
}
