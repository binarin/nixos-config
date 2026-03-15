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
        self.nixosModules.systemd-boot
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
          description = "Maximum memory allocation in MB.";
        };

        balloon = lib.mkOption {
          type = lib.types.nullOr lib.types.int;
          default = null;
          description = ''
            Minimum memory in MB for balloon device. When set lower than memory,
            enables dynamic memory ballooning. Setting to 0 disables the balloon driver.
            When null (default), Proxmox uses its default behavior.
          '';
        };

        shares = lib.mkOption {
          type = lib.types.int;
          default = 1000;
          description = ''
            Memory shares for auto-ballooning priority (0-50000).
            Higher values give the VM higher priority when host memory is constrained.
            Default is 1000 (Proxmox default).
          '';
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

        tpm2 = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable TPM2 emulation. Requires UEFI (bios=ovmf).";
          };

          version = lib.mkOption {
            type = lib.types.enum [
              "v1.2"
              "v2.0"
            ];
            default = "v2.0";
            description = "TPM version.";
          };

          storage = lib.mkOption {
            type = lib.types.str;
            default = "local-zfs";
            description = "Storage for TPM state.";
          };
        };

        efidisk = {
          storage = lib.mkOption {
            type = lib.types.str;
            default = "local-zfs";
            description = "Storage for EFI vars (used when bios=ovmf).";
          };

          efitype = lib.mkOption {
            type = lib.types.enum [
              "2m"
              "4m"
            ];
            default = "4m";
            description = "EFI disk size type (4m recommended for modern systems).";
          };

          secureBoot = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = ''
              Enable Secure Boot by pre-enrolling Microsoft and distribution keys.
              When false (default), the EFI disk is created without pre-enrolled keys,
              which disables Secure Boot and simplifies custom kernel/bootloader setups.
            '';
          };
        };

        cloudInit = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = true;
            description = "Enable cloud-init for initial configuration.";
          };

          storage = lib.mkOption {
            type = lib.types.str;
            default = "local-zfs";
            description = "Storage for cloud-init drive.";
          };
        };

        pci-passthrough = lib.mkOption {
          type = lib.types.attrsOf (
            lib.types.submodule {
              options = {
                id = lib.mkOption {
                  type = lib.types.nullOr lib.types.str;
                  default = null;
                  description = "lspci description substring to match. Mutually exclusive with mapping.";
                };

                mapping = lib.mkOption {
                  type = lib.types.nullOr lib.types.str;
                  default = null;
                  description = "Proxmox mapped PCI device name. Mutually exclusive with id.";
                };

                pci-express = lib.mkOption {
                  type = lib.types.bool;
                  default = true;
                  description = "Present device as PCIe (vs legacy PCI).";
                };

                primary-gpu = lib.mkOption {
                  type = lib.types.bool;
                  default = false;
                  description = "Mark as primary GPU (x-vga=1 in Proxmox).";
                };

                all-functions = lib.mkOption {
                  type = lib.types.bool;
                  default = false;
                  description = "Pass all functions of the device (multifunction). Address uses bus:slot without .function suffix.";
                };

                rom-bar = lib.mkOption {
                  type = lib.types.bool;
                  default = true;
                  description = "Enable ROM BAR mapping.";
                };

                rom = lib.mkOption {
                  type = lib.types.nullOr lib.types.path;
                  default = null;
                  description = "Path to ROM file. Uploaded to /usr/share/kvm/ on the Proxmox host during provisioning.";
                };
              };
            }
          );
          default = { };
          description = "PCI device passthrough configuration. Keys are labels sorted alphabetically for stable hostpciN assignment.";
        };
      };

      config = {
        services.qemuGuest.enable = lib.mkDefault config.nixos-config.qemu-guest.proxmox.agent;

        boot.kernelParams = [ "console=tty0" "console=ttyS0,115200n8" ];
        systemd.services."serial-getty@ttyS0".enable = true;

        assertions =
          let
            proxmox = config.nixos-config.qemu-guest.proxmox;
            diskoDisks = config.disko.devices.disk or { };
            # Check that disko device paths match the configured disk bus types.
            # virtio disks appear as /dev/vdX (no by-id entry), while scsi/sata
            # disks appear as /dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-<bus><index>
            virtioLetters = [ "a" "b" "c" "d" "e" "f" ];
            diskoBusAssertions = lib.concatMap (
              disk:
              let
                busKey = "${disk.bus}${toString disk.index}";
                isVirtio = disk.bus == "virtio";
                expectedVirtioDevice = "/dev/vd${builtins.elemAt virtioLetters disk.index}";
                expectedSuffix = "drive-${busKey}";
              in
              lib.mapAttrsToList (
                name: diskoCfg:
                let
                  device = diskoCfg.device or "";
                in
                {
                  assertion =
                    if isVirtio then
                      device == expectedVirtioDevice
                    else
                      !(lib.hasInfix "QEMU_HARDDISK" device) || lib.hasSuffix expectedSuffix device;
                  message =
                    if isVirtio then
                      "Disko disk '${name}' device '${device}' doesn't match proxmox virtio disk. Expected '${expectedVirtioDevice}'."
                    else
                      "Disko disk '${name}' device '${device}' doesn't match proxmox disk bus '${busKey}'. Expected path ending in '${expectedSuffix}'.";
                }
              ) diskoDisks
            ) proxmox.disks;
          in
          [
            {
              assertion =
                proxmox.tpm2.enable
                -> proxmox.bios == "ovmf";
              message = "TPM2 requires UEFI boot (bios = ovmf)";
            }
            {
              assertion =
                let
                  balloon = proxmox.balloon;
                  memory = proxmox.memory;
                in
                balloon == null || balloon <= memory;
              message = "Balloon memory must be less than or equal to maximum memory";
            }
          ]
          ++ (lib.concatLists (lib.mapAttrsToList (label: entry: [
            {
              assertion = (entry.id != null) != (entry.mapping != null);
              message = "pci-passthrough.${label}: exactly one of 'id' or 'mapping' must be set";
            }
          ]) proxmox.pci-passthrough))
          ++ diskoBusAssertions;
      };
    };
}
