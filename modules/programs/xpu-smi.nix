{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.xpu-smi = pkgs.callPackage ../../packages/xpu-smi { };
    };

  flake.nixosModules.xpu-smi =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        (pkgs.callPackage ../../packages/xpu-smi { })
      ];

      hardware.graphics.extraPackages = with pkgs; [
        intel-compute-runtime.drivers
      ];

    };
}
