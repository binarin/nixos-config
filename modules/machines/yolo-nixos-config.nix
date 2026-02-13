# modules/machines/yolo-nixos-config.nix
{ self, inputs, ... }:
{
  flake.nixosConfigurations.yolo-nixos-config = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inventoryHostName = "yolo-nixos-config";
    };
    modules = [
      self.nixosModules.yolo-nixos-config-configuration
    ];
  };

  flake.nixosModules.yolo-nixos-config-configuration =
    { ... }:
    {
      key = "nixos-config.modules.nixos.yolo-nixos-config-configuration";
      imports = [
        self.nixosModules.qemu-guest
        self.nixosModules.default
      ];

      config = {
        nixos-config.export-metrics.enable = false;
        networking.hostName = "yolo-nixos-config";
        system.stateVersion = "25.11";

        # Proxmox VM metadata (not used by NixOS, for VM creation script)
        nixos-config.qemu-guest.proxmox = {
          memory = 32768; # 32GB
          cores = 16;
          disks = [
            {
              type = "passthrough";
              device = "/dev/disk/by-id/ata-MZ7LM960HCHP-000V3_00YC381_00YC384LEN_91X5908M";
              bus = "scsi";
              index = 0;
              bootOrder = 1;
            }
          ];
          onboot = true;
          agent = true;
          description = "YOLO NixOS configuration testing VM";
        };

        boot.loader.grub.device = "/dev/disk/by-id/ata-MZ7LM960HCHP-000V3_00YC381_00YC384LEN_91X5908M";
        fileSystems."/" = {
          device = "/dev/disk/by-id/ata-MZ7LM960HCHP-000V3_00YC381_00YC384LEN_91X5908M-part1";
          fsType = "ext4";
        };
      };
    };
}
