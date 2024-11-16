{ config, pkgs, lib, ... }:
{
  options = {
    fonts.nerdfonts = lib.mkOption {
      type = with lib.types; listOf nonEmptyStr;
      default = [ ];
      description = ''
        Which nerd fonts variants to install. When you don't want to
        pull every nerd font variant.
      '';
    };
  };
  config = lib.mkIf config.hostConfig.feature.gui {
    fonts.fontconfig.enable = true;

    fonts.nerdfonts = [
      "FiraCode"
      "FiraMono"
      "Inconsolata"
      "InconsolataLGC" # cyrillic
      "Iosevka"
      "IosevkaTerm"
      "JetBrainsMono"
      "LiberationMono"
      "Noto"
      "RobotoMono"
      "SourceCodePro"
      "Terminus"
      "UbuntuMono"
    ];

    home.packages = with pkgs; [
      corefonts
      font-awesome
      vistafonts
      (nerdfonts.override { fonts = config.fonts.nerdfonts; })
    ];
  };
}
