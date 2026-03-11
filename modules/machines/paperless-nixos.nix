{
  self,
  inputs,
  config,
  ...
}:
let
  flakeConfig = config;
  inventoryHostName = "paperless-nixos";
  system = "x86_64-linux";
  paperlessTags = builtins.fromJSON (builtins.readFile ../services/paperless.json);
  publicKeys = import "${self}/inventory/public-keys.nix";
in
{
  flake.deploy.nodes.paperless-nixos = {
    hostname = config.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.paperless-nixos;
    };
  };

  flake.nixosConfigurations.paperless-nixos = inputs.nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = {
      inherit inventoryHostName;
      flake = {
        inherit self inputs config;
      };
    };
    modules = [
      self.nixosModules.paperless-nixos-configuration
    ];
  };

  flake.nixosModules.paperless-nixos-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (lib) mkDefault;
    in
    {
      key = "nixos-config.modules.nixos.paperless-nixos-configuration";
      imports = [
        "${self}/machines/paperless-nixos/hardware-configuration.nix"

        self.nixosModules.lxc
        self.nixosModules.baseline
        self.nixosModules.tailscale
        inputs.arion.nixosModules.arion
        inputs.sops-nix.nixosModules.sops
      ];

      config = {
        networking.hostName = inventoryHostName;
        system.stateVersion = "25.11";
        nixos-config.export-metrics.enable = true;

        # Docker and Arion configuration
        virtualisation.docker.enable = true;
        virtualisation.docker.autoPrune.enable = true;
        virtualisation.arion.backend = "docker";

        # LXC mount configuration for Proxmox
        proxmoxLXC = {
          cores = 4;
          memory = 8192;
          mounts = [
            {
              mountPoint = "/mnt/paperless";
              size = "128G";
            }
          ];
        };

        # Create paperless user with fixed UID from inventory (matches old host for migration)
        # Uses "users" group (GID 100) to match the original setup
        users.users.paperless = {
          isSystemUser = true;
          group = "users"; # GID 100, same as old host
          uid = flakeConfig.inventory.usersGroups.systemUsers.paperless.uid;
          # Home inside chroot, relative to chroot root
          home = "/mnt/paperless/sftp-chroot/consume";
          shell = pkgs.bashInteractive;
          openssh.authorizedKeys.keys = [
            publicKeys.ssh_keys.brother-scanner.public_key
          ];
        };

        # Brother scanner is really old
        services.openssh.settings.HostKeyAlgorithms = "+ssh-rsa";
        services.openssh.settings.PubkeyAcceptedAlgorithms = "+ssh-rsa";
        services.openssh.settings.Macs = [ "+hmac-sha2-256" ];

        # SFTP chroot configuration for Brother scanner
        # The scanner uploads to /consume within the chroot (/mnt/paperless/sftp-chroot)
        # ChrootDirectory must be owned by root with no write permission for others
        services.openssh.extraConfig = ''
          Match User paperless
            ChrootDirectory /mnt/paperless/sftp-chroot
            ForceCommand internal-sftp -l VERBOSE
            AllowTcpForwarding no
            X11Forwarding no
            PasswordAuthentication no
        '';

        # Sops secrets configuration
        sops.secrets."paperless/secret-key" = { };
        sops.secrets."paperless/postgres-password" = { };

        # Sops template for environment variables
        sops.templates."paperless-env" = {
          content = ''
            PAPERLESS_SECRET_KEY=${config.sops.placeholder."paperless/secret-key"}
            PAPERLESS_REDIS=redis://broker:6379
            PAPERLESS_DBHOST=db
            PAPERLESS_DBPASS=${config.sops.placeholder."paperless/postgres-password"}
            USERMAP_UID=${toString flakeConfig.inventory.usersGroups.systemUsers.paperless.uid}
            USERMAP_GID=${toString flakeConfig.inventory.usersGroups.systemUsers.paperless.gid}
            PAPERLESS_OCR_LANGUAGES=eng nld rus
            PAPERLESS_TIME_ZONE=Europe/Amsterdam
            PAPERLESS_OCR_LANGUAGE=nld+eng+rus
            PAPERLESS_CONSUMER_RECURSIVE=true
            PAPERLESS_CONSUMER_SUBDIRS_AS_TAGS=true
            PAPERLESS_URL=https://paperless.lynx-lizard.ts.net
            PAPERLESS_TIKA_ENABLED=1
            PAPERLESS_TIKA_GOTENBERG_ENDPOINT=http://gotenberg:3000
            PAPERLESS_TIKA_ENDPOINT=http://tika:9998
            PAPERLESS_CONSUMER_ENABLE_BARCODES=true
            PAPERLESS_CONSUMER_ENABLE_ASN_BARCODE=true
            PAPERLESS_CONSUMER_BARCODE_SCANNER=ZXING
          '';
          restartUnits = [ "paperless-docker-compose.service" ];
        };

        sops.templates."paperless-db-env" = {
          content = ''
            POSTGRES_DB=paperless
            POSTGRES_USER=paperless
            POSTGRES_PASSWORD=${config.sops.placeholder."paperless/postgres-password"}
          '';
          restartUnits = [ "paperless-docker-compose.service" ];
        };

        # Systemd tmpfiles rules for directory creation
        # Note: consume is inside sftp-chroot for SFTP access, symlinked to by Docker
        systemd.tmpfiles.rules = [
          # Main mount point
          "d /mnt/paperless 0755 root root -"

          # SFTP chroot directory - must be root owned for ChrootDirectory to work
          "d /mnt/paperless/sftp-chroot 0755 root root -"
          # Consume directory inside chroot - writable by paperless user
          "d /mnt/paperless/sftp-chroot/consume 0755 paperless users -"
          "d /mnt/paperless/sftp-chroot/consume/government 0755 paperless users -"
          "d /mnt/paperless/sftp-chroot/consume/purchases 0755 paperless users -"

          # Paperless application directories
          "d /mnt/paperless/data 0755 paperless users -"
          "d /mnt/paperless/media 0755 paperless users -"
          "d /mnt/paperless/export 0755 paperless users -"

          # Database directories
          "d /mnt/paperless/var 0755 root root -"
          "d /mnt/paperless/var/postgres 0755 999 999 -"
          "d /mnt/paperless/var/redis 0755 999 999 -"
        ];

        # Arion docker-compose configuration
        virtualisation.arion.projects.paperless = {
          serviceName = "paperless-docker-compose";
          settings = {
            services = {
              # Main Paperless webserver
              webserver = {
                service = {
                  image = "ghcr.io/paperless-ngx/paperless-ngx:${paperlessTags.webserver}";
                  container_name = "paperless-webserver";
                  restart = "unless-stopped";
                  ports = [ "8000:8000" ];
                  volumes = [
                    "/mnt/paperless/data:/usr/src/paperless/data"
                    "/mnt/paperless/media:/usr/src/paperless/media"
                    "/mnt/paperless/export:/usr/src/paperless/export"
                    # Consume directory is inside sftp-chroot for scanner access
                    "/mnt/paperless/sftp-chroot/consume:/usr/src/paperless/consume"
                  ];
                  env_file = [
                    config.sops.templates.paperless-env.path
                  ];
                  depends_on = [
                    "db"
                    "broker"
                    "gotenberg"
                    "tika"
                  ];
                };
              };

              # PostgreSQL database
              db = {
                service = {
                  image = "docker.io/library/postgres:${paperlessTags.db}";
                  container_name = "paperless-db";
                  restart = "unless-stopped";
                  expose = [ "5432" ];
                  volumes = [
                    "/mnt/paperless/var/postgres:/var/lib/postgresql/data"
                  ];
                  env_file = [
                    config.sops.templates.paperless-db-env.path
                  ];
                };
              };

              # Redis message broker
              broker = {
                service = {
                  image = "docker.io/library/redis:${paperlessTags.broker}";
                  container_name = "paperless-broker";
                  restart = "unless-stopped";
                  expose = [ "6379" ];
                  volumes = [
                    "/mnt/paperless/var/redis:/data"
                  ];
                };
              };

              # Gotenberg for document conversion
              gotenberg = {
                service = {
                  image = "docker.io/gotenberg/gotenberg:${paperlessTags.gotenberg}";
                  container_name = "paperless-gotenberg";
                  restart = "unless-stopped";
                  expose = [ "3000" ];
                  command = [
                    "gotenberg"
                    "--libreoffice-auto-start=true"
                    "--libreoffice-start-timeout=400s"
                    "--api-timeout=600s"
                    "--chromium-disable-javascript=true"
                  ];
                };
              };

              # Apache Tika for text extraction
              tika = {
                service = {
                  image = "docker.io/apache/tika:${paperlessTags.tika}";
                  container_name = "paperless-tika";
                  restart = "unless-stopped";
                  expose = [ "9998" ];
                };
              };
            };
          };
        };

        sops.secrets.tailscale-auth = { };
        services.tailscale = {
          authKeyFile = "${config.sops.secrets.tailscale-auth.path}";
        };

        # Expose Paperless as a Tailscale service
        services.tailscale.serve = {
          enable = mkDefault true;
          services.paperless = {
            serviceName = "paperless";
            protocol = "https";
            target = "localhost:8000";
          };
        };
      };
    };
}
