{ flake, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    self.nixosModules.default
    self.nixosModules.user-binarin
    ./configuration.nix
  ];

  inventoryHostName = "ishamael";

  hostConfig.features = [
    "hyprland"
    "nix-builder"
    "interactive-cli"
    "emacs"
    "tailscale"
  ];

  # Move XDG directories to .xdg subdirectory
  environment.sessionVariables = {
    XDG_CACHE_HOME = "$HOME/.xdg/cache";
    XDG_CONFIG_HOME = "$HOME/.xdg/config";
    XDG_DATA_HOME = "$HOME/.xdg/local/share";
    XDG_STATE_HOME = "$HOME/.xdg/local/state";
  };

  home-manager.users.binarin = {
    xdg = {
      enable = true;
      cacheHome = ".xdg/cache";
      configHome = ".xdg/config";
      dataHome = ".xdg/local/share";
      stateHome = ".xdg/local/state";
    };
  };

}
