{ self, ... }:
let
  protectedVars = import (self + "/lib/systemd-env-protected.nix");
in
{
  perSystem =
    { pkgs, ... }:
    {
      packages.ksso = pkgs.callPackage ../../packages/ksso {
        inherit protectedVars;
      };
    };

  flake.overlays.ksso = final: prev: {
    ksso = final.callPackage ../../packages/ksso {
      inherit protectedVars;
    };
  };
}
