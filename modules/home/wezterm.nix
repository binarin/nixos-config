{flake, lib, pkgs, config, ...}:
let
  defEnable = config.hostConfig.lib.defaults.enable;
in {
  config = lib.mkIf config.hostConfig.feature.gui {
    programs.wezterm = {
      enable = defEnable;
      enableZshIntegration = defEnable;
    };
    # xdg.configFile."wezterm/wezterm.lua".source = lib.mkForce (config.lib.file.mkOutOfStoreSymlink "/home/binarin/personal-workspace/nixos-config/files/wezterm.lua");
    xdg.configFile."wezterm/wezterm.lua".source = lib.mkForce (config.lib.self.file "wezterm.lua");
  };
}
