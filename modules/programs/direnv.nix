{ inputs, ... }:
{

  flake.homeModules.direnv =
    { config, pkgs, ... }:
    {
      key = "nixos-config.modules.home.direnv";
      config = {
        programs.direnv = {
          enable = true;
          enableZshIntegration = true;
          enableBashIntegration = true;
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
