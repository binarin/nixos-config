{ flake, ... }: final: prev: {
  inherit (final.bleeding) font-awesome noto-fonts-emoji nerdfonts;
}
