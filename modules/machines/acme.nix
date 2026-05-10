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
  flake.deploy.nodes.acme = {
    hostname = "acme";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.acme;
    };
  };

  clan.inventory.machines.acme = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.acme.home.primary.address;
  };

  clan.inventory.instances.acme = {
    module = {
      name = "lets-encrypt";
      input = "self";
    };
    roles.server.machines.acme = { };
    roles.server.settings = {
      acme.email = "binarin@binarin.info";
      acme.acceptTerms = true;
      acme.server = "https://acme-v02.api.letsencrypt.org/directory";
    };

  };

  clan.machines.acme = {
    imports = [
      self.nixosModules.acme-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosModules.acme-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.acme-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.qemu-guest
        self.nixosModules.disko-template-zfs-whole
        self.nixosModules.disko-template-add-uefi-boot-to-main
        (selfLib.file' "machines/llm-runner/hardware-configuration.nix")
      ];

      disko.devices.disk.main.device = "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0";
      nixos-config.qemu-guest.proxmox = {
        memory = 2048;
        balloon = 512;
        cores = 2;
        bios = "ovmf";
        machine = "q35";
        description = "ACME";
        tpm2.enable = true;
        disks = [
          {
            type = "image";
            storage = "local-zfs";
            size = "32G";
            bootOrder = 1;
          }
        ];
      };

      impermanence.enable = true;
      nixos-config.export-metrics.enable = true;
    };
}
