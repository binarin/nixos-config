{flake, config, lib, pkgs, ...}:
{
  config = lib.mkIf config.hostConfig.feature.impermanence {
    xdg = {
      enable = true;
      cacheHome = "${config.home.homeDirectory}/.xdg/cache";
      configHome = "${config.home.homeDirectory}/.xdg/config";
      dataHome = "${config.home.homeDirectory}/.xdg/local/share";
      stateHome = "${config.home.homeDirectory}/.xdg/local/state";
    };
  };
}
