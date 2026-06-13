{ self, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.klaude = pkgs.callPackage ../../packages/klaude { };
    };

  flake.overlays.klaude = final: prev: {
    klaude = final.callPackage ../../packages/klaude { };
  };
}
