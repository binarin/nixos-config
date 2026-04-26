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
  flake.deploy.nodes.pi-box = {
    hostname = "pi-box";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.pi-box;
    };
  };

  clan.inventory.machines.pi-box = {
    deploy.targetHost =
      if flakeConfig.inventory.ipAllocation ? pi-box then
        flakeConfig.inventory.ipAllocation.pi-box.home.primary.address
      else
        "0.0.0.0";
  };

  clan.machines.pi-box = {
    imports = [
      self.nixosModules.pi-box-configuration
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
  };

  flake.nixosModules.pi-box-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.pi-box-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
        self.nixosModules.binarin-workstation
      ];

      impermanence.enable = true;

      proxmoxLXC = {
        cores = 8;
        memory = 16384;
      };

      nixos-config.export-metrics.enable = false;
    };
}
