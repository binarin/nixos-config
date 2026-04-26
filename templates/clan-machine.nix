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
  flake.deploy.nodes.template = {
    hostname = "template";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.template;
    };
  };

  clan.inventory.machines.template = {
    deploy.targetHost =
      if flakeConfig.inventory.ipAllocation ? template then
        flakeConfig.inventory.ipAllocation.template.home.primary.address
      else
        "0.0.0.0";
  };

  clan.machines.template = {
    imports = [
      self.nixosModules.template-configuration
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
  };

  flake.nixosModules.template-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.template-configuration";
      imports = [
        self.nixosModules.baseline
      ];

      nixos-config.export-metrics.enable = false;
    };
}
