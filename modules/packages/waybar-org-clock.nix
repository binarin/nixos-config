{ self, ... }:
let
  selfLib = self.lib.self;
  packageFn =
    { rustPlatform, lib }:
    rustPlatform.buildRustPackage {
      pname = "waybar-org-clock";
      version = "0.1.0";
      src = selfLib.dir' "packages/waybar-org-clock";
      cargoLock.lockFile = selfLib.file' "packages/waybar-org-clock/Cargo.lock";
      meta.mainProgram = "waybar-org-clock";
    };
in
{
  perSystem =
    { pkgs, ... }:
    {
      packages.waybar-org-clock = pkgs.callPackage packageFn { };
    };

  flake.overlays.waybar-org-clock = final: prev: {
    waybar-org-clock = final.callPackage packageFn { };
  };
}
