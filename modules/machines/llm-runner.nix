{
  self,
  inputs,
  config,
  ...
}:
let
  inventoryHostName = "llm-runner";
  system = "x86_64-linux";
in
{
  flake.deploy.nodes.llm-runner = {
    hostname = config.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.llm-runner;
    };
  };

  flake.nixosConfigurations.llm-runner = inputs.nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = {
      inherit inventoryHostName;
      flake = {
        inherit self inputs config;
      };
    };
    modules = [
      self.nixosModules.llm-runner-configuration
    ];
  };

  flake.nixosModules.llm-runner-configuration =
    { ... }:
    {
      key = "nixos-config.modules.nixos.llm-runner-configuration";
      imports = [
        "${self}/machines/llm-runner/hardware-configuration.nix"

        self.nixosModules.disko
        self.nixosModules.baseline
        self.nixosModules.impermanence
        self.nixosModules.qemu-guest

      ];

      config = {
        networking.hostName = inventoryHostName;
        system.stateVersion = "25.11";
        nixos-config.export-metrics.enable = false;

        impermanence.enable = true;
        fileSystems."/persist".neededForBoot = true;
        fileSystems."/local".neededForBoot = true;
        nixos-config.qemu-guest.proxmox = {
          memory = 16384;
          balloon = 2048;
          disks = [
            {
              type = "image";
              storage = "local-zfs";
              size = "256G";
              bus = "virtio";
              bootOrder = 1;
            }
          ];
          bios = "ovmf";
          tpm2.enable = true;
        };
      };
    };
}
