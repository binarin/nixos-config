{ self, ... }:
{
  flake.nixosModules.gnupg =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.nixos.gnupg";

      programs.gnupg = {
        agent.enable = true;
        agent.pinentryPackage = pkgs.pinentry-curses;
      };

    };

  flake.homeModules.gnupg =
    { osConfig, pkgs, ... }:
    {
      key = "nixos-config.modules.home.gnupg";

      imports = [
        self.homeModules.impermanence
      ];

      impermanence.persist-directories = [
        {
          directory = ".gnupg";
          mode = "0700";
        }
      ];

      services.gpg-agent = {
        enable = true;
        defaultCacheTtl = 3600;
        maxCacheTtl = 14400;
        extraConfig = ''
          allow-preset-passphrase
        '';
        pinentry.package =
          if osConfig.services.graphical-desktop.enable then pkgs.pinentry-gtk2 else pkgs.pinentry-curses;
      };
    };
}
