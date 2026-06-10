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
  flake.deploy.nodes.docker-on-clan = {
    hostname = "docker-on-clan";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.docker-on-clan;
    };
  };

  clan.inventory.machines.docker-on-clan = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.docker-on-clan.home.primary.address;
  };

  clan.machines.docker-on-clan = {
    imports = [
      self.nixosModules.docker-on-clan-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  clan.inventory.instances.acme = {
    roles.client.machines.docker-on-clan = {
      settings = {
        domain = "docker-on-clan.clan.binarin.info";
        extraDomainNames = [
          "brick-tracker.binarin.info"
          "homebox.binarin.info"
        ];
      };
    };
  };

  flake.nixosConfigurations.docker-on-clan = lib.mkForce (
    self.clan.nixosConfigurations.docker-on-clan.extendModules {
      specialArgs.inventoryHostName = "docker-on-clan";
    }
  );

  flake.nixosModules.docker-on-clan-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.docker-on-clan-configuration";

      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
        self.nixosModules.impermanence
        "${inputs.nixpkgs}/nixos/modules/profiles/minimal.nix"
        inputs.arion.nixosModules.arion

        self.nixosModules.karakeep
        self.nixosModules.archivebox
      ];

      config = {
        networking.hostName = "docker-on-clan";

        impermanence.enable = true;
        system.stateVersion = "26.05";

        proxmoxLXC = {
          cores = 4;
          memory = 8192;
          rootfs.size = "8G";
          mounts = lib.mkForce [
            {
              mountPoint = "/nix";
              size = "8G";
              backup = true;
            }
            {
              mountPoint = "/persist";
              size = "8G";
              backup = true;
            }
            {
              mountPoint = "/local";
              size = "1G";
              backup = false;
            }
          ];
        };

        virtualisation.docker.enable = true;
        virtualisation.docker.autoPrune.enable = true;
        virtualisation.arion.backend = "docker";

        environment.persistence."/persist".directories = [
          "/var/lib/docker"
        ];

        # nginx reverse proxy with clan ACME certs (replaces caddy + cloudflare)
        services.nginx.enable = true;
        networking.firewall.allowedTCPPorts = [
          80
          443
        ];

        services.nginx.virtualHosts."brick-tracker.binarin.info" = {
          addSSL = true;
          sslCertificate = "/var/lib/ssl-cert/full.pem";
          sslCertificateKey = "/var/lib/ssl-cert/full.pem";
          locations."/" = {
            proxyPass = "http://localhost:3333";
          };
        };

        services.nginx.virtualHosts."homebox.binarin.info" = {
          addSSL = true;
          sslCertificate = "/var/lib/ssl-cert/full.pem";
          sslCertificateKey = "/var/lib/ssl-cert/full.pem";
          locations."/" = {
            proxyPass = "http://localhost:7745";
          };
        };

        environment.systemPackages = with pkgs; [ litecli ];

        # brick-tracker configuration
        sops.secrets."bricktracker/rebrickable-api-key" = { };
        sops.templates."bricktracker-env".content = ''
          BK_REBRICKABLE_API_KEY="${config.sops.placeholder."bricktracker/rebrickable-api-key"}"
        '';

        virtualisation.arion.projects.bricktracker = {
          serviceName = "bricktracker-docker-compose";
          settings = {
            services =
              let
                tags = builtins.fromJSON (builtins.readFile ./bricktracker.json);
              in
              {
                bricktracker = {
                  service = {
                    container_name = "BrickTracker";
                    restart = "unless-stopped";
                    image = "gitea.baerentsen.space/frederikbaerentsen/bricktracker:${tags.bricktracker}";
                    ports = [ "3333:3333" ];
                    volumes = [
                      "/persist/BrickTracker/data:/data/"
                      "/persist/BrickTracker/instructions:/app/static/instructions/"
                      "/persist/BrickTracker/minifigures:/app/static/minifigures/"
                      "/persist/BrickTracker/parts:/app/static/parts/"
                      "/persist/BrickTracker/sets:/app/static/sets/"
                    ];
                    environment = {
                      BK_DATABASE_PATH = "/data/app.db";
                      BK_MINIFIGURES_FOLDER = "minifigures";
                      BK_RETIRED_SETS_PATH = "/data/retired_sets.csv";
                      BK_THEMES_PATH = "/data/themes.csv";
                    };
                    env_file = [
                      config.sops.templates.bricktracker-env.path
                    ];
                  };
                };
              };
          };
        };

        # homebox configuration
        virtualisation.arion.projects.homebox = {
          serviceName = "home-box-docker-compose";
          settings = {
            services =
              let
                tags = builtins.fromJSON (builtins.readFile ./homebox.json);
              in
              {
                homebox = {
                  service = {
                    image = "ghcr.io/sysadminsmedia/homebox:${tags.homebox}";
                    container_name = "homebox";
                    restart = "unless-stopped";
                    environment = {
                      HBOX_LOG_LEVEL = "info";
                      HBOX_LOG_FORMAT = "text";
                      HBOX_WEB_MAX_FILE_UPLOAD = "10";
                      HBOX_OPTIONS_ALLOW_REGISTRATION = "false";
                      HBOX_MODE = "production";
                      HBOX_STORAGE_DATA = "/data";
                      HBOX_DATABASE_DRIVER = "sqlite3";
                      HBOX_STORAGE_SQLITE_PATH = "/data/homebox.db?_pragma=busy_timeout=999&_pragma=journal_mode=WAL&_fk=1";
                    };
                    volumes = [
                      "/persist/homebox/data:/data/"
                    ];
                    ports = [
                      "7745:7745"
                    ];
                  };
                };
              };
          };
        };

        nixos-config.export-metrics.enable = true;
      };
    };
}
