{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    console.useLargeFonts = lib.mkEnableOption "Use large console fonts (for HiDPI screens)";
  };

  config = lib.mkIf config.console.useLargeFonts {
    console = {
      earlySetup = true;
      font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
      packages = with pkgs; [ terminus_font ];
      keyMap = "us";
    };
  };
}
