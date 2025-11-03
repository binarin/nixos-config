{self, ...}: {
  flake.homeModules.direnv = { config, ...}: {
    key = "nixos-config.programs.direnv";
    config = {
      programs.direnv = {
        enable = true;
        enableZshIntegration = config.programs.zsh.enable;
        nix-direnv = {
          enable = true;
        };
        config.global = {
          # Make direnv messages less verbose
          hide_env_diff = true;
        };
      };

      impermanence.local-directories = [ "${config.xdg.dataHomeRelative}/direnv" ];
    };
  };
}
