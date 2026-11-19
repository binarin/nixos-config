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
    {
      lib,
      osConfig,
      pkgs,
      ...
    }:
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
          if osConfig.services.graphical-desktop.enable then pkgs.pinentry-qt else pkgs.pinentry-curses;
      };

      programs.gpg = {
        enable = true;
        # Don't manage gpg.conf: the module's mkDefault hardening set is not wanted.
        settings = lib.mkForce { };
        # disable-ccid: go through pcscd, so ykman/age-plugin-yubikey can
        # still see the reader. scdaemon still holds the card exclusively
        # (no pcsc-shared): `gpgconf --kill scdaemon` before using ykman.
        # disable-application piv: keep gpg to the OpenPGP application.
        scdaemonSettings = {
          disable-ccid = true;
          disable-application = "piv";
        };
      };
    };
}
