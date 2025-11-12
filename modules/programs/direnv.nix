{ ... }:
{
  flake.homeModules.direnv =
    { config, ... }:
    {
      key = "nixos-config.modules.home.direnv";
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

        impermanence.local-directories = [ ".local/share/direnv" ];
      };
    };
}
