{pkgs, ...}:

{
  fonts.fonts = with pkgs; [
    corefonts
    dejavu_fonts
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
    powerline-fonts
    roboto
    roboto-mono
    roboto-slab
    source-code-pro
    terminus_font_ttf
    ubuntu_font_family
    unifont
    vistafonts
  ] ++ lib.optionals (system == "x86_64-linux") [
    terminus_font
    google-fonts
  ];
}
