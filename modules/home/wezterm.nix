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

    xdg.dataFile."applications/org.wezfurlong.wezterm.desktop".source = pkgs.writeText "org.wezfurlong.wezterm.desktop" ''
      [Desktop Entry]
      Name=WezTerm
      Comment=Wez's Terminal Emulator
      Keywords=shell;prompt;command;commandline;cmd;
      Icon=org.wezfurlong.wezterm
      StartupWMClass=org.wezfurlong.wezterm
      TryExec=wezterm
      Exec=wezterm
      Type=Application
      Categories=System;TerminalEmulator;Utility;
      Terminal=false
    '';
  };
}
