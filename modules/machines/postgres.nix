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

      nixos-config.export-metrics.enable = false;
    };
}
