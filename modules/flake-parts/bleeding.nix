{ self, inputs, ... }:
{
  nixosSharedModules = [ self.nixosModules.bleeding ];

  flake.nixosModules.bleeding =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.bleeding";

      config = {
        nixpkgs.overlays = [
          (_final: prev: {
            bleeding = import inputs.nixpkgs-unstable {
              inherit (prev) system;
              config = config.nixpkgs.config;
              overlays = [
                # (bf: bp: {
                #   # diverges too fast, leads to segfaults. a lot of rebuilds is the price.
                #   # mesa = final.mesa.override {
                #   #   libdrm = final.bleeding.libdrm;
                #   # };
                # })
              ];
            };
          })
        ];
      };
    };
}
