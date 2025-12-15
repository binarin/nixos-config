{
  self,
  inputs,
  ...
}:
{
  flake.nixosConfigurations.forgejo = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.forgejo-configuration
    ];

  };

  flake.nixosModules.forgejo-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.forgejo-configuration";
      imports = [
        self.nixosModules.default
        self.nixosModules.lxc
      ];

      config = {
        networking.hostName = "forgejo";

        sops.secrets.tailscale-auth = { };
        services.tailscale = {
          enable = true;
          authKeyFile = "${config.sops.secrets.tailscale-auth.path}";
          extraUpFlags = [
            "--hostname"
            "${config.networking.hostName}"
          ];
        };

        environment.systemPackages = with pkgs; [
          emacs-nox
          git-crypt
        ];

        nix.gc = {
          automatic = true;
          dates = "weekly";
          options = "--delete-older-than 30d";
        };

        sops.secrets.smtp2go-username = { };
        sops.secrets.smtp2go-password = { };

        users.users.git = {
          home = config.services.forgejo.stateDir;
          group = "forgejo";
          useDefaultShell = true;
          isSystemUser = true;
        };

        services.forgejo = {
          enable = true;
          package = pkgs.forgejo; # at least 9.0, for SSH keys in push mirror
          user = "git";

          settings = {
            service = {
              DISABLE_REGISTRATION = true;
            };
            repository = {
              DEFAULT_BRANCH = "master";
              DEFAULT_PRIVATE = "private";
            };
            server = {
              HTTP_ADDR = "127.0.0.1";
              DOMAIN = "forgejo.lynx-lizard.ts.net";
              ROOT_URL = "https://forgejo.lynx-lizard.ts.net/";
              SSH_TRUSTED_USER_CA_KEYS = lib.concatStringsSep "," (config.lib.publicKeys.secureWithTag "user-ca");
              SSH_AUTHORIZED_PRINCIPALS_ALLOW = "username";
              SSH_CREATE_AUTHORIZED_PRINCIPALS_FILE = true;
            };
            mailer = {
              ENABLED = true;
              PROTOCOL = "smtps";
              SMTP_ADDR = "mail.smtp2go.com";
              SMTP_PORT = 465;
              FROM = "host-forgejo@binarin.info";
            };
            time = {
              DEFAULT_UI_LOCATION = "Europe/Amsterdam";
            };
          };
          secrets = {
            mailer = {
              USER = "${config.sops.secrets.smtp2go-username.path}";
              PASSWD = "${config.sops.secrets.smtp2go-password.path}";
            };
          };
        };

        services.openssh.extraConfig = ''
          Match User git
          AuthorizedPrincipalsFile %h/.ssh/authorized_principals
          AuthorizedKeysFile %h/.ssh/authorized_keys
        '';

        system.stateVersion = "24.05";
        nixos-config.export-metrics.enable = true;
      };
    };
}
