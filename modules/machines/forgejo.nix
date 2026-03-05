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
        self.nixosModules.baseline
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

        environment.systemPackages =
          let
            cfg = config.services.forgejo;
            forgejo-cli = pkgs.writeScriptBin "forgejo-cli" ''
              #!${pkgs.runtimeShell}
              cd ${cfg.stateDir}
              sudo=exec
              if [[ "$USER" != ${cfg.user} ]]; then
                sudo='exec /run/wrappers/bin/sudo -u ${cfg.user} -g ${cfg.group} --preserve-env=FORGEJO_WORK_DIR --preserve-env=FORGEJO_CUSTOM'
              fi
              export FORGEJO_WORK_DIR=${cfg.stateDir}
              export FORGEJO_CUSTOM=${cfg.customDir}
              $sudo ${lib.getExe cfg.package} "$@"
            '';
          in
          [
            forgejo-cli
            pkgs.emacs-nox
            pkgs.git-crypt
          ];

        nix.gc = {
          automatic = true;
          dates = "weekly";
          options = "--delete-older-than 30d";
        };

        sops.secrets.smtp2go-username = { };
        sops.secrets.smtp2go-password = { };
        sops.secrets."garage/key-id" = { };
        sops.secrets."garage/secret-key" = { };

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
            storage = {
              STORAGE_TYPE = "minio";
              MINIO_USE_SSL = true;
              MINIO_ENDPOINT = "s3.lynx-lizard.ts.net";
              MINIO_BUCKET = "forgejo-artifacts";
              MINIO_LOCATION = "garage";
            };
            attachment.MINIO_BASE_PATH = "attachments/";
            lfs.MINIO_BASE_PATH = "lfs/";
            avatar.MINIO_BASE_PATH = "avatars/users/";
            repo-avatar.MINIO_BASE_PATH = "avatars/repositories/";
            repo-archive.MINIO_BASE_PATH = "archives/";
            packages.MINIO_BASE_PATH = "packages/";
            "storage.actions_log".MINIO_BASE_PATH = "actions/logs/";
            "actions.artifacts".MINIO_BASE_PATH = "actions/artifacts/";
          };
          secrets = {
            storage = {
              MINIO_ACCESS_KEY_ID = "${config.sops.secrets."garage/key-id".path}";
              MINIO_SECRET_ACCESS_KEY = "${config.sops.secrets."garage/secret-key".path}";
            };
            mailer = {
              USER = "${config.sops.secrets.smtp2go-username.path}";
              PASSWD = "${config.sops.secrets.smtp2go-password.path}";
            };
          };
        };

        systemd.services.forgejo-regenerate-keys =
          let
            cfg = config.services.forgejo;
            exe = lib.getExe cfg.package;
          in
          {
            description = "Regenerate Forgejo authorized_keys and principals";
            after = [ "forgejo.service" ];
            bindsTo = [ "forgejo.service" ];
            wantedBy = [ "forgejo.service" ];
            serviceConfig = {
              Type = "oneshot";
              User = cfg.user;
              Group = cfg.group;
              WorkingDirectory = cfg.stateDir;
              ExecStart = "${exe} admin regenerate keys";
            };
            environment = {
              USER = cfg.user;
              HOME = cfg.stateDir;
              FORGEJO_WORK_DIR = cfg.stateDir;
              FORGEJO_CUSTOM = cfg.customDir;
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
