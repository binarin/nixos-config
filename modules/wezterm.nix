{
  ...
}:
{
  flake.homeModules.wezterm =
    {
      lib,
      pkgs,
      config,
      osConfig,
      ...
    }:
    let
      propagatedConfig = pkgs.writeText "stylix-vars.lua" ''
        local vars = {
          fontName = ${lib.escapeShellArg (builtins.toString config.stylix.fonts.monospace.name)},
          fontSize = ${builtins.toString config.stylix.fonts.sizes.terminal},
        }
        return vars
      '';
    in
    {
      key = "nixos-config.modules.home.wezterm";

      config = lib.mkIf osConfig.services.graphical-desktop.enable {
        programs.wezterm = {
          enable = true;
          enableZshIntegration = true;
        };

        # xdg.configFile."wezterm/wezterm.lua".source = lib.mkForce (config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/files/wezterm.lua");
        xdg.configFile."wezterm/wezterm.lua".source = lib.mkForce (config.lib.self.file "wezterm.lua");
        xdg.configFile."wezterm/stylix-vars.lua".source = propagatedConfig;

        # NOTE: Needed because default .desktop has some arguments that prevents (re)connection to an existing server
        xdg.dataFile."applications/org.wezfurlong.wezterm.desktop".source =
          pkgs.writeText "org.wezfurlong.wezterm.desktop" ''
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
    };
}
