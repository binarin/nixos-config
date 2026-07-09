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
        group = "postgres";
        reloadServices = [ "postgresql.service" ];
      };
    };
  };

  clan.inventory.machines.postgres = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.postgres.home.primary.address;
  };

  clan.inventory.instances.postgres = {
    module = {
      input = "self";
      name = "postgresql";
    };
    roles.server.machines.postgres = { };
    roles.client.machines.postgres.settings.access =
      let
        tsCIDRs = [
          "100.64.0.0/10"
          "fd7a:115c:a1e0::/48"
        ];
      in
      {
        hledger = {
          database = "hledger";
          user = "hledger";
          role = "owner";
          sourceCIDRs = tsCIDRs;
        };
        hledger-rw = {
          database = "hledger";
          user = "hledger_rw";
          role = "readwrite";
          sourceCIDRs = tsCIDRs;
        };
        hledger-ro = {
          database = "hledger";
          user = "hledger_ro";
          role = "readonly";
          sourceCIDRs = tsCIDRs;
        };
      };
    roles.client.machines.media.settings.access = {
      atuin = {
        database = "atuin";
        user = "atuin";
        role = "owner";
        sourceCIDRs = [
          "100.64.0.0/10"
          "fd7a:115c:a1e0::/48"
        ];
      };
    };
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
        package = pkgs.postgresql_18;
        settings = {
          shared_buffers = "2GB";
          effective_cache_size = "6GB";
        };
      };

      nixos-config.export-metrics.enable = false;
    };
}
