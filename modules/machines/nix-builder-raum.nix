{
  self,
  inputs,
  config,
  lib,
  ...
}:
let
  inventoryHostName = "nix-builder-raum";
  system = "x86_64-linux";
  flakeConfig = config;
in
{
  flake.deploy.nodes.nix-builder-raum = {
    hostname = config.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.nix-builder-raum;
    };
  };

  clan.inventory.machines.nix-builder-raum = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.nix-builder-raum.home.primary.address;
  };

  clan.machines.nix-builder-raum = {
    imports = [
      self.nixosModules.nix-builder-raum-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  clan.inventory.instances.forgejo-runner.roles.runner.machines.nix-builder-raum.settings = {
    count = 2;
    hostPackages = [ inputs.niks3.packages.x86_64-linux.niks3 ];
    supplementaryGroups = [
      "podman"
      "nix-access-tokens"
    ];
  };

  flake.nixosConfigurations.nix-builder-raum = lib.mkForce (
    self.clan.nixosConfigurations.nix-builder-raum.extendModules {
      specialArgs.inventoryHostName = "nix-builder-raum";
    }
  );

  flake.nixosModules.nix-builder-raum-configuration =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.nix-builder-raum-configuration";
      imports = [
        "${self}/my-machines/nix-builder-raum/hardware-configuration.nix"

        self.nixosModules.lxc
        self.nixosModules.baseline
        self.nixosModules.nix-builder
      ];

      config = {
        networking.hostName = inventoryHostName;
        system.stateVersion = "25.11";

        proxmoxLXC = {
          cores = 8;
          memory = 32768;
          mounts = [
            {
              mountPoint = "/nix";
              size = "512G";
            }
            {
              mountPoint = "/var/lib/private/gitea-runner";
              size = "32G";
            }
          ];
        };
      };
    };
}
