{ self, ... }:
{
  flake.homeModules.binarin-direnv =
    { ... }:
    {
      key = "nixos-config.modules.home.binarin-direnv";
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
      };
    };

  flake.homeModules.direnv =
    { ... }:
    {
      key = "nixos-config.modules.home.direnv";
      imports = [
        self.homeModules.binarin-direnv
      ];
      config = {
        impermanence.local-directories = [ ".local/share/direnv" ];
      };
    };
}
