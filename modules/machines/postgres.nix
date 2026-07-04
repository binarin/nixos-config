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
        };
      };

      nixos-config.export-metrics.enable = false;
    };
}
