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
  flake.deploy.nodes.metabase = {
    hostname = "metabase";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.metabase;
    };
  };

  clan.inventory.instances.acme.roles.client.machines.metabase = {
    settings = {
      domain = "metabase.home.binarin.info";
      extraDomainNames = [ "metabase.clan.binarin.info" ];
      reloadServices = [ "nginx.service" ];
    };
  };

  clan.inventory.machines.metabase = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.metabase.home.primary.address;
  };

  clan.inventory.instances.postgres.roles.client.machines.metabase.settings.access.metabase = {
    role = "owner";
    sourceCIDRs = [
      "100.64.0.0/10"
      "${flakeConfig.inventory.ipAllocation.metabase.home.primary.address}/32"
    ];
    restartUnits = [ "metabase.service" ];
  };

  clan.machines.metabase = {
    imports = [
      self.nixosModules.metabase-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosConfigurations.metabase = lib.mkForce (
    self.clan.nixosConfigurations.metabase.extendModules {
      specialArgs.inventoryHostName = "metabase";
    }
  );

  flake.nixosModules.metabase-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.metabase-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
      ];

      config = {
        proxmoxLXC = {
          cores = 4;
          memory = 4096;
          mounts = [
            {
              mountPoint = "/nix";
              size = "32G";
              backup = false;
            }
          ];
        };

        services.metabase = {
          enable = true;
          listen.port = 3000;
        };

        clan.core.vars.generators.metabase-encryption = {
          files.secret-key = {
            secret = true;
            deploy = true;
            restartUnits = [ "metabase.service" ];
          };
          runtimeInputs = [ pkgs.openssl ];
          script = ''
            openssl rand -base64 32 | tr -d '\n' > $out/secret-key
          '';
        };

        systemd.services =
          let
            dbPasswordFile = config.clan.core.vars.generators.postgresql-postgres-metabase-metabase.files.password.path;
            secretKeyFile = config.clan.core.vars.generators.metabase-encryption.files.secret-key.path;
          in
          {
            metabase-secrets = {
              description = "Render Metabase secrets EnvironmentFile";
              wantedBy = [ "multi-user.target" ];
              before = [ "metabase.service" ];
              requiredBy = [ "metabase.service" ];
              serviceConfig = {
                Type = "oneshot";
                RemainAfterExit = true;
                RuntimeDirectory = "metabase-secrets";
                UMask = "0077";
              };
              script = ''
                password=$(<${dbPasswordFile})
                secretKey=$(<${secretKeyFile})
                {
                  printf 'MB_DB_CONNECTION_URI=postgres://postgres.lynx-lizard.ts.net:5432/metabase?user=metabase&password=%s&sslmode=require\n' "$password"
                  printf 'MB_ENCRYPTION_SECRET_KEY=%s\n' "$secretKey"
                } > /run/metabase-secrets/env
              '';
            };
            metabase.serviceConfig = {
              EnvironmentFile = [ "/run/metabase-secrets/env" ];
              Environment = [
                "MB_LLM_ANTHROPIC_API_BASE_URL=http://aperture.lynx-lizard.ts.net"
                "MB_LLM_ANTHROPIC_MODEL=deepseek/deepseek-v4-pro"
                "MB_LLM_ANTHROPIC_API_KEY=-"
                "MB_LLM_MAX_TOKENS=16384"
              ];
            };
          };

        services.tailscale = {
          enable = true;
          serve = {
            enable = true;
            configs.metabase-web = {
              protocol = "https";
              target = "localhost:3000";
            };
          };
        };

        services.nginx = {
          enable = true;
          virtualHosts."metabase.home.binarin.info" = {
            forceSSL = true;
            enableACME = false;
            serverAliases = [ "metabase.clan.binarin.info" ];
            sslCertificate = "/var/lib/ssl-cert/full.pem";
            sslCertificateKey = "/var/lib/ssl-cert/full.pem";
            listen = [
              {
                addr = config.inventory.hostIpAllocation.home.primary.address;
                port = 80;
              }
              {
                addr = config.inventory.hostIpAllocation.home.primary.address;
                port = 443;
                ssl = true;
              }
              {
                addr = "127.0.0.1";
                port = 80;
              }
              {
                addr = "127.0.0.1";
                port = 443;
                ssl = true;
              }
            ];
            locations."/" = {
              proxyPass = "http://localhost:3000";
              recommendedProxySettings = true;
            };
          };
        };

        nixos-config.export-metrics.enable = true;
      };
    };
}
