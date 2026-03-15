{
  self,
  config,
  lib,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
in
{
  clan.inventory.machines.llm-runner = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.llm-runner.home.primary.address;
  };

  clan.machines.llm-runner = {
    imports = [
      self.nixosModules.llm-runner-configuration
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
  };

  flake.nixosConfigurations.llm-runner = lib.mkForce (
    self.clan.nixosConfigurations.llm-runner.extendModules {
      specialArgs.inventoryHostName = "llm-runner";
    }
  );

  flake.nixosModules.llm-runner-configuration =
    { ... }:
    {
      key = "nixos-config.modules.nixos.llm-runner-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.qemu-guest
        self.nixosModules.disko-template-zfs-whole
        (selfLib.file' "machines/llm-runner/hardware-configuration.nix")
      ];

      nixos-config.export-metrics.enable = false;

      nixos-config.qemu-guest.proxmox = {
        memory = 65536;
        balloon = 2048;
        cores = 16;
        bios = "ovmf";
        machine = "q35";
        description = "LLM runner";
        pci-passthrough = {
          nvme = {
            id = "Samsung Electronics Co Ltd NVMe SSD Controller SM981/PM981/PM983";
            bootable = true;
          };
          # gpu = {
          #   id = "NVIDIA Corporation GA102 [GeForce RTX 3090] (rev a1)";
          #   # primary-gpu = true;
          #   rom = selfLib.file "GA102.rom.git-crypt";
          # };
          # gpu-sound = {
          #   id = "NVIDIA Corporation GA102 High Definition Audio Controller (rev a1)";
          # };
          # radeon = {
          #   mapping = "large-radeon";
          # };
        };
      };

      impermanence.enable = true;
      disko.devices.disk.main.device =
        "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_1TB_S4EWNF0M723324Z";
    };
}
