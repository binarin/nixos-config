{
  self,
  inputs,
  config,
  lib,
  ...
}:
let
  inventoryHostName = "nix-builder-valak";
  system = "x86_64-linux";
  flakeConfig = config;
in
{
  flake.deploy.nodes.nix-builder-valak = {
    hostname = config.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.nix-builder-valak;
    };
  };

  clan.inventory.machines.nix-builder-valak = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.nix-builder-valak.home.primary.address;
  };

  clan.machines.nix-builder-valak = {
    imports = [
      self.nixosModules.nix-builder-valak-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  clan.inventory.instances.forgejo-runner.roles.runner.machines.nix-builder-valak.settings = {
    count = 3;
    hostPackages = [ inputs.niks3.packages.x86_64-linux.niks3 ];
    supplementaryGroups = [
      "podman"
      "nix-access-tokens"
    ];
  };

  flake.nixosConfigurations.nix-builder-valak = lib.mkForce (
    self.clan.nixosConfigurations.nix-builder-valak.extendModules {
      specialArgs.inventoryHostName = "nix-builder-valak";
    }
  );

  flake.nixosModules.nix-builder-valak-configuration =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.nix-builder-valak-configuration";
      imports = [
        "${self}/my-machines/nix-builder-valak/hardware-configuration.nix"

        self.nixosModules.lxc
        self.nixosModules.baseline
        self.nixosModules.nix-builder
      ];

      config = {
        networking.hostName = inventoryHostName;
        system.stateVersion = "25.11";

        proxmoxLXC = {
          cores = 16;
          memory = 49152;
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
