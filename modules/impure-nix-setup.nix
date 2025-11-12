{ self, inputs, ... }:
{
  flake.nixosModules.impure-nix-setup =
    { config, lib, ... }:
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

        environment.etc."nix/inputs/nixpkgs".source = "${inputs.nixpkgs}";
        environment.etc."nix/inputs/self-absolute-path".text = "${self}";
        environment.etc."nix/overlays/self-overlays.nix".source =
          config.lib.self.file "nixos-configuration-overlays-for-impure-nix-commands.nix";
        environment.etc."nix/nixpkgs-config.nix".source =
          config.lib.self.file "nixos-configuration-nix-config-for-impure-nix-commands.nix";

        environment.variables.NIXPKGS_CONFIG = "/etc/nix/nixpkgs-config.nix";

        nix.nixPath = [
          "nixpkgs=/etc/nix/inputs/nixpkgs"
          "nixpkgs-overlays=/etc/nix/overlays"
        ];

        nix.registry.nixpkgs.flake = inputs.nixpkgs;
        nix.registry.nixpkgs-unstable.flake = inputs.nixpkgs-unstable;
        nix.registry.flake-parts.flake = inputs.flake-parts;
      };
    };
}
