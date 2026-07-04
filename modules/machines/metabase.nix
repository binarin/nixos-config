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

        systemd.services.metabase = {
          environment = {
            MB_DB_TYPE = "postgres";
            MB_DB_HOST = "postgres.lynx-lizard.ts.net";
            MB_DB_PORT = "5432";
            MB_DB_DBNAME = "metabase";
            MB_DB_USER = "metabase";
          };
          serviceConfig.LoadCredential = [
            "MB_DB_PASS:${config.clan.core.vars.generators.metabase-db.files.password.path}"
          ];
          serviceConfig.ImportCredential = [ "MB_DB_PASS" ];
        };


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
