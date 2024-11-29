{flake, config, pkgs, lib, ...}:

{
  config = lib.mkIf config.hostConfig.feature.impermanence {
    environment.sessionVariables = {
      XDG_CACHE_HOME = "$HOME/.xdg/cache";
      XDG_CONFIG_HOME = "$HOME/.xdg/config";
      XDG_DATA_HOME = "$HOME/.xdg/local/share";
      XDG_STATE_HOME = "$HOME/.xdg/local/state";
    };
  };
}
