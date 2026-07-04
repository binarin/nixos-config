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
  flake.deploy.nodes.postgres = {
    hostname = "postgres";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.postgres;
    };
  };

  clan.inventory.instances.acme = {
    roles.client.machines.postgres = {
      settings = {
        domain = "postgres.home.binarin.info";
        reloadServices = [ "postgresql.service" ];
      };
    };
  };

  clan.inventory.machines.postgres = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.postgres.home.primary.address;
  };

  clan.machines.postgres = {
    imports = [
      self.nixosModules.postgres-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosConfigurations.postgres = lib.mkForce (
    self.clan.nixosConfigurations.postgres.extendModules {
      specialArgs.inventoryHostName = "postgres";
    }
  );

  flake.nixosModules.postgres-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.postgres-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
        self.nixosModules.metabase-db-generator
      ];

      proxmoxLXC = {
        cores = 4;
        memory = 8192;
        mounts = [
          {
            mountPoint = "/nix";
            size = "32G";
            backup = false;
          }
          {
            mountPoint = "/var/lib/postgresql";
            size = "128G";
            backup = true;
          }
        ];
      };

      services.postgresql = {
        enable = true;
        package = pkgs.postgresql_18;
        settings = {
          shared_buffers = "2GB";
          effective_cache_size = "6GB";
          ssl = "on";
          ssl_cert_file = "/var/lib/ssl-cert/full.pem";
          ssl_key_file = "/var/lib/ssl-cert/full.pem";
        };
      };

      services.postgresql.authentication = ''
        hostssl metabase metabase 192.168.2.36/32 md5
        host    metabase metabase 100.64.0.0/10  md5
      '';

      clan.core.postgresql = {
        enable = true;
        users.metabase = { };
        databases.metabase = { };
      };

      systemd.services.metabase-db-password = {
        description = "Set metabase database user password";
        wantedBy = [ "multi-user.target" ];
        requires = [ "postgresql.service" ];
        after = [ "postgresql.service" ];
        path = [ config.services.postgresql.package ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          User = "postgres";
          Group = "postgres";
        };
        script = ''
          export PGPASSFILE=/run/postgresql/.pgpass
          psql -c "ALTER USER metabase WITH PASSWORD '$(cat ${config.clan.core.vars.generators.metabase-db.files.password.path})'"
        '';
      };

      nixos-config.export-metrics.enable = false;
    };
}
