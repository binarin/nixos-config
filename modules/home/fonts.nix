{ config, pkgs, lib, ... }:

{
  config = lib.mkIf config.gui.enable {
    fonts.fontconfig.enable = true;
    home.packages = with pkgs; [
      corefonts
      # dejavu_fonts
      emacs-all-the-icons-fonts
      fira
      fira-code
      font-awesome
      inconsolata
      iosevka
      jetbrains-mono
      liberation_ttf
      # mplus-outline-fonts
      noto-fonts
      noto-fonts-emoji
      powerline-fonts
      roboto
      roboto-mono
      roboto-slab
      source-code-pro
      terminus_font_ttf
      ubuntu_font_family
      unifont
      vistafonts
      (nerdfonts.override { fonts = [ "Noto" ]; })
    ];
  };
}
