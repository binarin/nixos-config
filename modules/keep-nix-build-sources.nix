{ self, ... }:
{
  nixosSharedModules = [ self.nixosModules.keep-nix-build-sources ];

  flake.nixosModules.keep-nix-build-sources =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.keep-nix-build-sources";

      config = lib.mkMerge [
        (lib.mkIf config.hostConfig.feature.nix-builder or false {
          # environment.etc."nix/flake-all-sources-keeper".source = "${pkgs.flake-all-sources-keeper}";
          # nixpkgs.overlays = [
          #   (final: prev: { flake-all-sources-keeper = flake-all-sources-keeper final; })
          # ];
          nix.extraOptions = ''
            gc-keep-outputs = true
            gc-keep-derivations = true
          '';
        })
      ];
    };
}
