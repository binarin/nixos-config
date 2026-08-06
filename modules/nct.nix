{ self, ... }:
let
  selfLib = self.lib.self;
  packageFn =
    { rustPlatform, lib }:
    rustPlatform.buildRustPackage {
      pname = "nct";
      version = "0.1.0";
      src = selfLib.dir' "tools/nct";
      cargoLock.lockFile = selfLib.file' "tools/nct/Cargo.lock";
      meta = {
        mainProgram = "nct";
        description = "nixos-config-tool: CLI for NixOS configuration management";
      };
    };
in
{
  perSystem =
    { pkgs, ... }:
    {
      packages.nct = pkgs.callPackage packageFn { };

      devShells.nct = pkgs.mkShell {
        name = "nct";
        packages = with pkgs; [
          cargo
          rustc
          rust-analyzer
          clippy
          rustfmt
        ];
      };
    };

  flake.overlays.nct = final: _prev: {
    nct = final.callPackage packageFn { };
  };
}
