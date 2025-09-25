{flake, lib, pkgs, config, ...}:
{
  config = {
    home.packages = with pkgs; [
      kmonad
    ];
  };
}
