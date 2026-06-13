{ self, ... }:
let
  protectedVars = import (self + "/lib/systemd-env-protected.nix");
in
{
  perSystem =
    { pkgs, ... }:
    {
      packages.jerk-gpa = pkgs.callPackage ../../packages/jerk-gpa {
        inherit protectedVars;
      };
    };

  flake.overlays.jerk-gpa = final: prev: {
    jerk-gpa = final.callPackage ../../packages/jerk-gpa {
      inherit protectedVars;
    };
  };
}
