# Add NixOS Machine - Reference

Complete module templates and inventory file formats.

## Module Templates

### Minimal qemu-guest

The simplest possible VM configuration:

```nix
# modules/machines/my-vm.nix
{ self, inputs, ... }:
{
  flake.nixosConfigurations.my-vm = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.my-vm-configuration
    ];
  };

  flake.nixosModules.my-vm-configuration =
    { modulesPath, ... }:
    {
      key = "nixos-config.modules.nixos.my-vm-configuration";
      imports = [
        (modulesPath + "/profiles/qemu-guest.nix")
        self.nixosModules.default
      ];

      config = {
        nixos-config.export-metrics.enable = false;
        networking.hostName = "my-vm";
        system.stateVersion = "25.11";

        boot.loader.grub.device = "/dev/vda";
        fileSystems."/" = {
          device = "/dev/vda1";
          fsType = "ext4";
        };
      };
    };
}
```

### qemu-guest with inventory integration

VM with inventory IP allocation:

```nix
# modules/machines/my-server.nix
{ self, inputs, ... }:
{
  flake.nixosConfigurations.my-server = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inventoryHostName = "my-server";
    };
    modules = [
      self.nixosModules.my-server-configuration
    ];
  };

  flake.nixosModules.my-server-configuration =
    { modulesPath, config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.my-server-configuration";
      imports = [
        (modulesPath + "/profiles/qemu-guest.nix")
        self.nixosModules.default
        self.nixosModules.srvos-bits
        self.nixosModules.tailscale
      ];

      config = {
        nixos-config.export-metrics.enable = true;
        networking.hostName = "my-server";
        system.stateVersion = "25.11";

        boot.loader.grub.device = "/dev/vda";
        fileSystems."/" = {
          device = "/dev/vda1";
          fsType = "ext4";
        };

        # Tailscale for remote access
        sops.secrets.tailscale-auth = { };
        services.tailscale = {
          enable = true;
          authKeyFile = config.sops.secrets.tailscale-auth.path;
        };
      };
    };
}
```

### qemu-guest with Proxmox metadata

VM configuration with Proxmox VM creation metadata:

```nix
# modules/machines/my-proxmox-vm.nix
{ self, inputs, ... }:
{
  flake.nixosConfigurations.my-proxmox-vm = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inventoryHostName = "my-proxmox-vm";
    };
    modules = [
      self.nixosModules.my-proxmox-vm-configuration
    ];
  };

  flake.nixosModules.my-proxmox-vm-configuration =
    { ... }:
    {
      key = "nixos-config.modules.nixos.my-proxmox-vm-configuration";
      imports = [
        self.nixosModules.qemu-guest  # Includes qemu profile + proxmox metadata options
        self.nixosModules.default
      ];

      config = {
        nixos-config.export-metrics.enable = true;
        networking.hostName = "my-proxmox-vm";
        system.stateVersion = "25.11";

        # Proxmox VM metadata (not used by NixOS, for VM creation script)
        nixos-config.qemu-guest.proxmox = {
          vmId = 200;  # Optional: can be specified at creation time with --vmid
          memory = 4096;  # 4GB in MB
          cores = 4;

          # For passthrough disk (physical device attached to VM)
          disks = [
            {
              type = "passthrough";
              device = "/dev/disk/by-id/ata-EXAMPLE_DISK_ID";
              bus = "scsi";
              index = 0;
              bootOrder = 1;
            }
          ];

          # OR for image-based disk (Proxmox-managed storage)
          # disks = [
          #   {
          #     type = "image";
          #     storage = "local-zfs";
          #     size = "32G";
          #     bus = "scsi";
          #     index = 0;
          #     bootOrder = 1;
          #   }
          # ];

          network = {
            bridge = "vmbr0";
            model = "virtio";
            firewall = true;
          };

          onboot = true;
          agent = true;
          description = "My NixOS Proxmox VM";
        };

        # Disk configuration should match proxmox metadata
        boot.loader.grub.device = "/dev/disk/by-id/ata-EXAMPLE_DISK_ID";
        fileSystems."/" = {
          device = "/dev/disk/by-id/ata-EXAMPLE_DISK_ID-part1";
          fsType = "ext4";
        };
      };
    };
}
```

#### Proxmox metadata options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `vmId` | int | null | Proxmox VM ID (can override with --vmid) |
| `memory` | int | 2048 | Memory in MB |
| `cores` | int | 2 | CPU cores |
| `sockets` | int | 1 | CPU sockets |
| `bios` | enum | "seabios" | BIOS type (seabios/ovmf) |
| `machine` | str | "q35" | Machine type |
| `scsihw` | str | "virtio-scsi-single" | SCSI hardware type |
| `onboot` | bool | false | Start on host boot |
| `agent` | bool | true | Enable QEMU guest agent |
| `description` | str | null | VM description |

#### Disk configuration

For passthrough disks (physical devices):
```nix
{
  type = "passthrough";
  device = "/dev/disk/by-id/...";
  bus = "scsi";  # or virtio, sata, ide
  index = 0;
  bootOrder = 1;  # optional
}
```

For image disks (Proxmox storage):
```nix
{
  type = "image";
  storage = "local-zfs";
  size = "32G";
  bus = "scsi";
  index = 0;
  bootOrder = 1;
}
```

#### Using proxmox-vm-create script

```bash
# Generate qm commands (dry run)
nix run .#proxmox-vm-create -- my-proxmox-vm --vmid 200 --dry-run

# Execute on Proxmox host via SSH
ssh root@valak "$(nix run .#proxmox-vm-create -- my-proxmox-vm --vmid 200 --dry-run)"
```

The script automatically adds `--serial0 socket --vga serial0` for NixOS console access.

### LXC Container

Proxmox LXC container configuration:

```nix
# modules/machines/my-lxc.nix
{ self, inputs, ... }:
{
  flake.nixosConfigurations.my-lxc = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.my-lxc-configuration
    ];
  };

  flake.nixosModules.my-lxc-configuration =
    { config, lib, pkgs, ... }:
    {
      key = "nixos-config.modules.nixos.my-lxc-configuration";
      imports = [
        self.nixosModules.default
        self.nixosModules.lxc
        self.nixosModules.binarin-baseline
      ];

      config = {
        networking.hostName = "my-lxc";
        system.stateVersion = "25.11";

        # LXC-specific settings are handled by self.nixosModules.lxc
      };
    };
}
```

### Bare-metal Workstation

Physical machine with full workstation setup:

```nix
# modules/machines/my-workstation.nix
{ self, inputs, ... }:
{
  flake.nixosConfigurations.my-workstation = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inventoryHostName = "my-workstation";
    };
    modules = [
      self.nixosModules.my-workstation-configuration
    ];
  };

  flake.nixosModules.my-workstation-configuration =
    { config, lib, pkgs, modulesPath, ... }:
    {
      key = "nixos-config.modules.nixos.my-workstation-configuration";
      imports = [
        self.nixosModules.baseline
        (modulesPath + "/installer/scan/not-detected.nix")
        "${self}/machines/my-workstation/hardware-configuration.nix"

        self.nixosModules.default
        self.nixosModules.binarin-workstation
        self.nixosModules.niri
        self.nixosModules.kanata
        self.nixosModules.bluetooth
        self.nixosModules.firefox
      ];

      config = {
        nixos-config.export-metrics.enable = false;
        networking.hostName = "my-workstation";
        system.stateVersion = "25.11";

        # Per-machine home-manager configuration
        home-manager.users.binarin = self.homeModules.my-workstation-binarin;

        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;

        # Kanata keyboard remapping device
        services.kanata.keyboards.all.devices = [
          "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
        ];
      };
    };

  # Per-machine home-manager configuration
  flake.homeModules.my-workstation-binarin =
    { ... }:
    {
      key = "nixos-config.modules.home.my-workstation-binarin";

      # Machine-specific home settings
      programs.waybar.battery = {
        enable = true;
        name = "BAT1";
      };
    };
}
```

## Inventory Files

### inventory/host-id.nix

Format:
```nix
{
  # hostId command: head -c4 /dev/urandom | od -A none -t x4 | perl -nE 'm,(\w+), && print $1'
  existing-machine = "12345678";
  new-machine = "abcd1234";  # Add new entries here
}
```

### inventory/networks/home.nix

Format:
```nix
{
  info = {
    prefix = 24;
    network = "192.168.2.0";
    gateway = "192.168.2.1";
    dns = [ "192.168.2.1" ];
    domain = "home.binarin.info";
  };

  ipam = {
    "192.168.2.1" = "gateway";
    "192.168.2.2" = "existing-machine";
    "192.168.2.3" = "new-machine";  # Add new allocation
    # "192.168.2.4" = "";  # Commented = available
    # "192.168.2.5" = "";
  };
}
```

**IP allocation patterns:**
- Simple: `"192.168.2.X" = "hostname";`
- With interface tag: `"192.168.2.X" = [ "hostname" "eth" ];`
- Available (commented out): `# "192.168.2.X" = "";`

## Common Import Combinations

### Server (headless)
```nix
imports = [
  self.nixosModules.default
  self.nixosModules.srvos-bits
  self.nixosModules.tailscale
];
```

### Workstation (graphical)
```nix
imports = [
  self.nixosModules.baseline
  self.nixosModules.default
  self.nixosModules.binarin-workstation
  self.nixosModules.niri
  self.nixosModules.kanata
  self.nixosModules.bluetooth
  self.nixosModules.firefox
];
```

### Container/VM with binarin user (minimal)
```nix
imports = [
  self.nixosModules.default
  self.nixosModules.lxc  # or qemu-guest profile
  self.nixosModules.binarin-baseline
];
```

### With impermanence
```nix
imports = [
  self.nixosModules.impermanence
  self.nixosModules.disko
  # ... other imports
];

impermanence.enable = true;
fileSystems."/persist".neededForBoot = true;
fileSystems."/local".neededForBoot = true;
```

## Key Configuration Attributes

### Required in every machine module
- `key = "nixos-config.modules.nixos.<name>-configuration";`
- `networking.hostName = "<name>";`
- `system.stateVersion = "X.Y";`

### Commonly needed
- `nixos-config.export-metrics.enable = true/false;`
- `boot.loader.*` - bootloader configuration
- `fileSystems."/"` - root filesystem

### For secrets
```nix
sops.secrets.secret-name = { };
# Use: config.sops.secrets.secret-name.path
```

### For Tailscale
```nix
sops.secrets.tailscale-auth = { };
services.tailscale = {
  enable = true;
  authKeyFile = config.sops.secrets.tailscale-auth.path;
};
```
