{ ... }:
{
  flake.nixosModules.large-console-fonts =
    {
      pkgs,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.large-console-fonts";

      options = {
        console.useLargeFonts = lib.mkEnableOption "Use large console fonts (for HiDPI screens)";
      };

      config = {
        console = {
          earlySetup = true;
          font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
          packages = with pkgs; [ terminus_font ];
          keyMap = "us";
        };
      };
    };
}
