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
  flake.deploy.nodes.zeroclaw = {
    hostname = flakeConfig.inventory.ipAllocation.zeroclaw.guest.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.zeroclaw;
    };
  };

  clan.inventory.machines.zeroclaw = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.zeroclaw.guest.primary.address;
  };

  clan.machines.zeroclaw = {
    imports = [ self.nixosModules.zeroclaw-configuration ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosConfigurations.zeroclaw = lib.mkForce (
    self.clan.nixosConfigurations.zeroclaw.extendModules {
      specialArgs.inventoryHostName = "zeroclaw";
    }
  );

  flake.nixosModules.zeroclaw-configuration =
    {
      config,
      lib,
      pkgs,
      modulesPath,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.zeroclaw-configuration";

      imports = [
        self.nixosModules.nixos-base
        self.nixosModules.qemu-guest
        self.nixosModules.provision-clan-key

        self.nixosModules.nix
        self.nixosModules.tailscale
        self.nixosModules.clan-tailscale
      ];

      boot.initrd.availableKernelModules = [
        "uhci_hcd"
        "ehci_pci"
        "ahci"
        "nvme"
        "sr_mod"
      ];

      fileSystems."/" = {
        device = "/dev/disk/by-label/nixos";
        fsType = "ext4";
        autoResize = true;
      };
      fileSystems."/boot" = {
        device = "/dev/disk/by-label/ESP";
        fsType = "vfat";
        # Without these the ESP mounts 0022, and bootctl warns that the mount
        # point backing /boot/loader/random-seed is world accessible.
        options = [
          "fmask=0077"
          "dmask=0077"
        ];
      };

      boot.growPartition = true;

      networking.useNetworkd = true;

      boot.initrd.provisionClanKey.enable = true;

      environment.systemPackages = with pkgs; [
        git

        tramp-rpc-server
        ripgrep

        htop
        btop
        bat
      ];

      system.build.cloudImage = import "${pkgs.path}/nixos/lib/make-disk-image.nix" {
        inherit lib pkgs config;
        format = "qcow2";
        partitionTableType = "efi";
        additionalSpace = "1024M";
      };

      nixos-config.qemu-guest.proxmox = {
        memory = 8192;
        network.inventoryNetwork = "guest";
        balloon = 512;
        cores = 4;
        bios = "ovmf";
        machine = "q35";
        description = "zeroclaw";
        disks = [
          {
            type = "image";
            storage = "local-zfs";
            size = "32G";
            bootOrder = 1;
          }
        ];
      };

      services.tailscale.enable = true;
    };
}
