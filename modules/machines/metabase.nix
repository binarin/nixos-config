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

  clan.inventory.instances.acme-metabase = {
    module = {
      input = "self";
      name = "lets-encrypt";
    };
    roles.client.machines.metabase = {
      settings = {
        domain = "metabase.home.binarin.info";
        extraDomainNames = [ "metabase.clan.binarin.info" ];
        reloadServices = [ "nginx.service" ];
      };
    };
  };

  clan.inventory.machines.metabase = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.metabase.home.primary.address;
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

        # SSL requires MB_DB_CONNECTION_URI; render it (with the password from the sops
        # placeholder) into a tmpfs EnvironmentFile so the secret never enters the store.
        sops.templates."metabase-db-uri.env" = {
          restartUnits = [ "metabase.service" ];
          content = ''
            MB_DB_CONNECTION_URI=postgres://postgres.lynx-lizard.ts.net:5432/metabase?user=metabase&password=${
              config.sops.placeholder."vars/postgresql-postgres-metabase-metabase/password"
            }&sslmode=require
          '';
        };

        systemd.services.metabase.serviceConfig.EnvironmentFile = [
          config.sops.templates."metabase-db-uri.env".path
        ];


        services.tailscale = {
          enable = true;
          serve = {
            services.metabase = {
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
            locations."/" = {
              proxyPass = "http://localhost:3000";
              recommendedProxySettings = true;
            };
          };
        };

        nixos-config.export-metrics.enable = false;
      };
    };
}
