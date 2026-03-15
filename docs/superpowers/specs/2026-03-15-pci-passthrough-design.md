# PCI Passthrough for Proxmox VMs

## Summary

Add PCI passthrough support to the NixOS VM provisioning system: define device options in `qemu-guest.nix`, resolve PCI addresses at provisioning time, and provide a subcommand to update existing VM configs.

## NixOS Options (`qemu-guest.nix`)

Add `pci-passthrough` option under `options.nixos-config.qemu-guest.proxmox`:

```nix
pci-passthrough = lib.mkOption {
  type = lib.types.attrsOf (lib.types.submodule {
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
        description = "Pass all functions of the device (multifunction). Address uses bus:slot without .function.";
      };
      rom-bar = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable ROM BAR mapping.";
      };
      rom = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "Path to ROM file. Will be uploaded to /usr/share/kvm/ on the Proxmox host.";
      };
    };
  });
  default = {};
  description = "PCI device passthrough configuration. Keys are labels sorted alphabetically for stable hostpciN assignment.";
};
```

Assertion: exactly one of `id` or `mapping` must be set per entry.

## llm-runner.nix Test Config

```nix
pci-passthrough = {
  nvme = {
    id = "Samsung Electronics Co Ltd NVMe SSD Controller SM981/PM981/PM983";
  };
  gpu = {
    id = "NVIDIA Corporation GA102 [GeForce RTX 3090] (rev a1)";
    primary-gpu = true;
    rom = selfLib.file "GA102.rom.git-crypt";
  };
  gpu-sound = {
    id = "NVIDIA Corporation GA102 High Definition Audio Controller (rev a1)";
  };
  radeon = {
    mapping = "large-radeon";
  };
};
```

Sorted labels: `gpu` (hostpci0), `gpu-sound` (hostpci1), `nvme` (hostpci2), `radeon` (hostpci3).

## Provisioning Logic (provision_vm_anywhere.py)

New step after disk configuration, before ISO attachment:

1. Read `pci-passthrough` from `vm_config`, sort entries by label.
2. For each entry, indexed as N:
   - **id mode**: SSH to Proxmox host, run `lspci`, find lines containing the `id` string. Extract bus:slot.function addresses. Fail if not exactly one match.
     - If `all-functions`: strip `.function` suffix to get `bus:slot` format.
   - **mapping mode**: use `mapping=<name>` syntax instead of address.
   - If `rom` is set: SCP the file to `/usr/share/kvm/<basename>` on Proxmox host.
   - Build hostpci spec string:
     - id mode: `<addr>,pcie=<0|1>,x-vga=<0|1>,rombar=<0|1>[,romfile=<basename>]`
     - mapping mode: `mapping=<name>,pcie=<0|1>,x-vga=<0|1>,rombar=<0|1>[,romfile=<basename>]`
   - Run `qm set <vmid> --hostpci<N> <spec>`.

### PCI Address Resolution

```python
def resolve_pci_device(proxmox_host: str, device_id: str, all_functions: bool) -> str:
    """Resolve lspci description to PCI bus address.

    Runs `lspci` on the Proxmox host, finds lines containing device_id.
    Fails if match count != 1.
    Returns address in format 0f:00.0 (or 0f:00 if all_functions).
    """
```

### ROM Upload

```python
def upload_rom_file(proxmox_host: str, rom_path: str) -> str:
    """Upload ROM file to /usr/share/kvm/ on Proxmox host via SCP.

    Returns the basename for use in romfile= parameter.
    """
```

## New Subcommand: `ncf machine update-proxmox-vm`

```
ncf machine update-proxmox-vm <machine> --proxmox-host <host> [--apply]
```

### Behavior

- Dry-run by default: shows proposed changes as diffs. `--apply` executes them.
- Checks VM is stopped via API. Fails if running.
- Finds VM by hostname match.

### Config Properties Updated

- `memory`, `balloon`, `shares`, `cores`, `sockets`, `description`
- `hostpci*` entries: resolves desired PCI config, compares with current, drops unknown/stale entries, adds/updates as needed.

### Diff Display

For each changed property, show:
```
  memory: 4096 -> 65536
  hostpci0: (none) -> 0f:00.0,pcie=1,x-vga=1,rombar=1,romfile=GA102.rom.git-crypt
  hostpci5: 03:00.0,pcie=1 -> (removed)
```

### Implementation

New file: `tools/ncf/ncf/commands/update_proxmox_vm.py`

Core logic:
1. Query NixOS config for desired state.
2. Find VM by hostname via Proxmox API.
3. Check VM is stopped.
4. Get current VM config.
5. Compute desired values for memory/cpu/pci properties.
6. Diff current vs desired.
7. If `--apply`: execute `qm set` commands and delete stale `hostpci*` entries.

### CLI Registration

In `cli.py`, register as `@machine_app.command("update-proxmox-vm")`.

## Files Modified

- `modules/qemu-guest.nix` — add `pci-passthrough` option + assertion
- `modules/machines/llm-runner.nix` — update pci-passthrough config (add radeon mapped device, fix ROM filename)
- `tools/ncf/ncf/commands/provision_vm_anywhere.py` — add PCI passthrough step
- `tools/ncf/ncf/commands/update_proxmox_vm.py` — new file
- `tools/ncf/ncf/cli.py` — register new subcommand, import new module

## Shared Code

PCI resolution and ROM upload functions go in `provision_vm_anywhere.py` or a shared utility. Since `update_proxmox_vm.py` also needs them, extract to a helper module `tools/ncf/ncf/commands/pci_passthrough.py`:
- `resolve_pci_device(proxmox_host, device_id, all_functions) -> str`
- `upload_rom_file(proxmox_host, rom_path) -> str`
- `build_hostpci_spec(entry, proxmox_host) -> str`
- `configure_pci_passthrough(proxmox_host, vmid, pci_config, dry_run) -> list[tuple[str, str]]` — returns list of (key, value) pairs applied
