{ self, ... }:
let
  selfLib = self.lib.self;
in
{
  perSystem =
    { pkgs, ... }:
    {
      packages.git-2_54 = pkgs.callPackage ../../packages/git-2.54 { };
    };

  flake.overlays.git-2_54 = final: prev: {
    git-2_54 = final.callPackage ../../packages/git-2.54 { };
  };
}
