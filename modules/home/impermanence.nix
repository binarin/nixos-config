{flake, config, pkgs, ...}:
{
  config = {
    xdg = {
      enable = true;
      cacheHome = "${config.home.homeDirectory}/.xdg/cache";
      configHome = "${config.home.homeDirectory}/.xdg/config";
      dataHome = "${config.home.homeDirectory}/.xdg/local/share";
      stateHome = "${config.home.homeDirectory}/.xdg/local/state";
    };
  };
}
