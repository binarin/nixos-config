{
  config,
  pkgs,
  lib,
  ...
}:
{
  config = lib.mkIf config.hostConfig.feature.gui {
    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      corefonts
      font-awesome
      vistafonts
      nerd-fonts.fira-code
      nerd-fonts.fira-mono
      nerd-fonts.inconsolata
      nerd-fonts.inconsolata-lgc # cyrillic
      nerd-fonts.iosevka
      nerd-fonts.iosevka-term
      nerd-fonts.jetbrains-mono
      nerd-fonts.liberation
      nerd-fonts.noto
      nerd-fonts.roboto-mono
      nerd-fonts.sauce-code-pro
      nerd-fonts.terminess-ttf
      nerd-fonts.ubuntu-mono
    ];
  };
}
