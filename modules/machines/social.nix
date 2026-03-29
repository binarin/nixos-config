{
  self,
  config,
  lib,
  inputs,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
in
{
  flake.deploy.nodes.social = {
    hostname = "social.home.binarin.info";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.social;
    };
  };

  clan.inventory.machines.social = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.social.home.primary.address;
  };

  clan.machines.social = {
    imports = [
      self.nixosModules.social-configuration
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
  };

  clan.inventory.instances.acme = {
    roles.client.machines.social = {
      settings = {
        domain = "social.clan.binarin.info";
        extraDomainNames = [
          "binarin.info"
          "social.home.binarin.info"
        ];
      };
    };
  };

  flake.nixosModules.social-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.social-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
        self.nixosModules.clan-smtp
      ];

      proxmoxLXC = {
        cores = 4;
        memory = 8192;
        mounts = [
          {
            mountPoint = "/nix";
            size = "512G";
            backup = false;
          }
        ];
      };

      nixos-config.export-metrics.enable = false;

      services.tailscale.enable = lib.mkForce false;

      clan.core.vars.generators.mastodon-vapid = {
        files.public-key = {
          secret = false;
        };
        files.private-key = {
          owner = config.services.mastodon.user;
        };
        runtimeInputs = with pkgs; [
          gawk
          gnugrep
        ];
        script = ''
          cd ${pkgs.mastodon}
          RAILS_ENV=production ./bin/rake webpush:generate_keys > $out/all
          cat $out/all | grep '^Public' | awk '{ print $3 }' > $out/public-key
          cat $out/all | grep '^Private' | awk '{ print $3 }' > $out/private-key
          rm $out/all
        '';
      };

      clan.core.vars.generators.mastodon-active-record = {
        files.deterministic-key = {
          owner = config.services.mastodon.user;
        };
        files.key-derivation-salt = {
          owner = config.services.mastodon.user;
        };
        files.primary-key = {
          owner = config.services.mastodon.user;
        };
        runtimeInputs = with pkgs; [
          perl
        ];
        script = ''
          cd ${pkgs.mastodon}
          RAILS_ENV=production ./bin/rails db:encryption:init > $out/all

          cat $out/all | perl -nE 'm/^ACTIVE_RECORD_ENCRYPTION_DETERMINISTIC_KEY=(.*)$/ && say $1' > $out/deterministic-key
          cat $out/all | perl -nE 'm/^ACTIVE_RECORD_ENCRYPTION_KEY_DERIVATION_SALT=(.*)$/ && say $1' > $out/key-derivation-salt
          cat $out/all | perl -nE 'm/^ACTIVE_RECORD_ENCRYPTION_PRIMARY_KEY=(.*)$/ && say $1' > $out/primary-key
          rm $out/all
        '';
      };

      users.users."${config.services.mastodon.user}".extraGroups = [
        "smtp-password"
      ];

      clan.core.vars.generators.mastodon-rails = {
        files.secret-key-base = {
          owner = config.services.mastodon.user;
        };
        script = ''
          cd ${pkgs.mastodon}
          RAILS_ENV=production ./bin/bundle exec rails secret > $out/secret-key-base
        '';
      };

      networking.firewall.allowedTCPPorts = [
        443
      ];

      services.nginx.virtualHosts."${config.services.mastodon.localDomain}" = {
        enableACME = false;
        serverAliases = [
          "social.home.binarin.info"
          "social.clan.binarin.info"
        ];
        sslCertificate = "/var/lib/ssl-cert/full.pem";
        sslCertificateKey = "/var/lib/ssl-cert/full.pem";
      };

      services.mastodon = {
        enable = true;
        configureNginx = true;
        localDomain = "binarin.info";
        streamingProcesses = 3;

        extraConfig = {
          SINGLE_USER_MODE = "true";
          ALTERNATE_DOMAINS = "social.home.binarin.info,social.clan.binarin.info";
        };

        vapidPublicKeyFile = config.clan.core.vars.generators.mastodon-vapid.files.public-key.path;
        vapidPrivateKeyFile = config.clan.core.vars.generators.mastodon-vapid.files.private-key.path;

        activeRecordEncryptionDeterministicKeyFile =
          config.clan.core.vars.generators.mastodon-active-record.files.deterministic-key.path;
        activeRecordEncryptionKeyDerivationSaltFile =
          config.clan.core.vars.generators.mastodon-active-record.files.key-derivation-salt.path;
        activeRecordEncryptionPrimaryKeyFile =
          config.clan.core.vars.generators.mastodon-active-record.files.primary-key.path;

        secretKeyBaseFile = config.clan.core.vars.generators.mastodon-rails.files.secret-key-base.path;

        smtp = {
          createLocally = false;
          host = "mail-eu.smtp2go.com";
          port = 2525;
          authenticate = true;
          fromAddress = "mastodon@binarin.info";
          user = "clan-binarin.info";
          passwordFile = config.clan.core.vars.generators.smtp.files.password.path;
        };
      };

    };
}
