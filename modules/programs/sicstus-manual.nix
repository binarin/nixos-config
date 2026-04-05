{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.sicstus-manual = pkgs.callPackage ../../packages/sicstus-manual { };
    };

  flake.overlays.sicstus-manual = final: prev: {
    sicstus-manual = final.callPackage ../../packages/sicstus-manual { };
  };
}
