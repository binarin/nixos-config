# modules/qemu-guest.nix
{ self, ... }:
{
  flake.nixosModules.qemu-guest =
    {
      modulesPath,
      config,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.qemu-guest";

      imports = [
        (modulesPath + "/profiles/qemu-guest.nix")
      ];

      options.nixos-config.qemu-guest.proxmox = {
        vmId = lib.mkOption {
          type = lib.types.nullOr lib.types.int;
          default = null;
          description = "Proxmox VM ID (100-999999). Can be overridden at creation time.";
        };

        memory = lib.mkOption {
          type = lib.types.int;
          default = 2048;
          description = "Memory allocation in MB.";
        };

        cores = lib.mkOption {
          type = lib.types.int;
          default = 2;
          description = "Number of CPU cores.";
        };

        sockets = lib.mkOption {
          type = lib.types.int;
          default = 1;
          description = "Number of CPU sockets.";
        };

        disks = lib.mkOption {
          type = lib.types.listOf (
            lib.types.submodule {
              options = {
                type = lib.mkOption {
                  type = lib.types.enum [
                    "passthrough"
                    "image"
                  ];
                  description = "Disk type: passthrough for raw device, image for Proxmox-managed storage.";
                };

                device = lib.mkOption {
                  type = lib.types.nullOr lib.types.str;
                  default = null;
                  description = "Device path for passthrough (e.g., /dev/disk/by-id/...).";
                };

                storage = lib.mkOption {
                  type = lib.types.nullOr lib.types.str;
                  default = null;
                  description = "Proxmox storage name for image type (e.g., local-zfs, local-lvm).";
                };

                size = lib.mkOption {
                  type = lib.types.nullOr lib.types.str;
                  default = null;
                  description = "Disk size for image type (e.g., 32G, 100G).";
                };

                bus = lib.mkOption {
                  type = lib.types.enum [
                    "scsi"
                    "virtio"
                    "sata"
                    "ide"
                  ];
                  default = "scsi";
                  description = "Disk bus type.";
                };

                index = lib.mkOption {
                  type = lib.types.int;
                  default = 0;
                  description = "Disk index (0 for scsi0, 1 for scsi1, etc.).";
                };

                bootOrder = lib.mkOption {
                  type = lib.types.nullOr lib.types.int;
                  default = null;
                  description = "Boot order priority (lower = higher priority).";
                };
              };
            }
          );
          default = [ ];
          description = "Disk configurations for the VM.";
        };

        network = lib.mkOption {
          type = lib.types.submodule {
            options = {
              bridge = lib.mkOption {
                type = lib.types.str;
                default = "vmbr0";
                description = "Network bridge name.";
              };

              model = lib.mkOption {
                type = lib.types.enum [
                  "virtio"
                  "e1000"
                  "rtl8139"
                ];
                default = "virtio";
                description = "Network card model.";
              };

              firewall = lib.mkOption {
                type = lib.types.bool;
                default = true;
                description = "Enable Proxmox firewall for this interface.";
              };
            };
          };
          default = { };
          description = "Network configuration.";
        };

        bios = lib.mkOption {
          type = lib.types.enum [
            "seabios"
            "ovmf"
          ];
          default = "seabios";
          description = "BIOS type (seabios for legacy, ovmf for UEFI).";
        };

        machine = lib.mkOption {
          type = lib.types.str;
          default = "q35";
          description = "Machine type (q35 recommended for modern VMs).";
        };

        scsihw = lib.mkOption {
          type = lib.types.str;
          default = "virtio-scsi-single";
          description = "SCSI hardware type.";
        };

        onboot = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Start VM on Proxmox host boot.";
        };

        agent = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Enable QEMU guest agent.";
        };

        description = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "VM description for Proxmox UI.";
        };
      };

      config = {
        services.qemuGuest.enable = lib.mkDefault config.nixos-config.qemu-guest.proxmox.agent;
      };
    };
}
