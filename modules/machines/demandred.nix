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

  flake.deploy.nodes.demandred = {
    hostname = "demandred";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.demandred;
    };
  };

  clan.inventory.machines.demandred = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.demandred.home.eth.address;
  };

  clan.machines.demandred = {
    imports = [
      self.nixosModules.demandred-configuration
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
  };

  flake.nixosModules.demandred-configuration =
    {
      lib,
      pkgs,
      modulesPath,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.demandred-configuration";

      imports = [
        self.nixosModules.baseline
      ];

      config = {
        networking.hostName = "demandred";

        impermanence.enable = true;
        fileSystems."/persist".neededForBoot = true;
        fileSystems."/local".neededForBoot = true;

        nixos-config.export-metrics.enable = false;
        system.stateVersion = "25.11";

        boot.initrd.systemd.enable = true;
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;

        networking.networkmanager.enable = true;
        networking.useDHCP = lib.mkDefault true;
      };
    };
}
