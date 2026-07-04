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

      nixos-config.export-metrics.enable = false;
    };
}
