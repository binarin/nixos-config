# PCI Passthrough for Proxmox VMs

## Summary

Add PCI passthrough support to the NixOS VM provisioning system: define device options in `qemu-guest.nix`, resolve PCI addresses at provisioning time, and provide a subcommand to update existing VM configs.

## NixOS Options (`qemu-guest.nix`)

Add `pci-passthrough` option under `options.nixos-config.qemu-guest.proxmox`, alongside existing options like `memory`, `cores`, etc.:

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

### Assertion

Exactly one of `id` or `mapping` must be set per entry:

```nix
(lib.concatLists (lib.mapAttrsToList (label: entry: [{
  assertion = (entry.id != null) != (entry.mapping != null);
  message = "pci-passthrough.${label}: exactly one of 'id' or 'mapping' must be set";
}]) proxmox.pci-passthrough))
```

## llm-runner.nix Desired Config

Note: the current working tree has `GAT102` (typo) which is corrected to `GA102` to match the actual file `files/GA102.rom.git-crypt`. The `radeon` entry is new.

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
   - **id mode**: SSH to Proxmox host, run `lspci` (plain, no `-D`), find lines containing the `id` string. Extract bus:slot.function address from the start of each matching line (before the first space). Fail if not exactly one match. The `id` string must be specific enough to match exactly one lspci output line.
     - If `all-functions`: strip `.function` suffix to get `bus:slot` format.
   - **mapping mode**: use `mapping=<name>` syntax instead of address.
   - If `rom` is set: SCP the file to `/usr/share/kvm/<machine>-pci-<label>.rom` on Proxmox host (e.g., `llm-runner-pci-gpu.rom`). Note: the `rom` value is a Nix store path (e.g., `/nix/store/...-GA102.rom.git-crypt`) since `lib.types.path` serializes that way via `query_nixos_config`.
   - Build hostpci spec string (address/mapping must come first):
     - id mode: `<addr>,pcie=<0|1>,x-vga=<0|1>,rombar=<0|1>[,romfile=<machine>-pci-<label>.rom]`
     - mapping mode: `mapping=<name>,pcie=<0|1>,x-vga=<0|1>,rombar=<0|1>[,romfile=<machine>-pci-<label>.rom]`
   - Run `qm set <vmid> --hostpci<N> <spec>`.

### PCI Address Resolution

```python
def resolve_pci_device(proxmox_host: str, device_id: str, all_functions: bool) -> str:
    """Resolve lspci description to PCI bus address.

    Runs plain `lspci` (no -D flag) on the Proxmox host via SSH.
    Finds lines containing device_id substring.
    Extracts address from start of line (before first space).
    Fails if match count != 1.
    Returns address in format 0f:00.0 (or 0f:00 if all_functions).
    """
```

### ROM Upload

```python
def upload_rom_file(proxmox_host: str, rom_path: str) -> str:
    """Upload ROM file to /usr/share/kvm/ on Proxmox host via SCP.

    rom_path is a Nix store path (from lib.types.path serialization).
    dest_name is generated as <machine>-pci-<label>.rom.
    Returns the dest_name for use in romfile= parameter.
    """
```

## New Subcommand: `ncf machine update-proxmox-vm`

```
ncf machine update-proxmox-vm <machine> --proxmox-host <host> [--apply]
```

### Behavior

- Dry-run by default: shows proposed changes as diffs. `--apply` executes them.
- Finds VM by hostname match using `client.vm_exists(hostname)` from `proxmox_api.py`.
- Checks VM is stopped via API. Fails if running.

### Config Properties Updated

- `memory`, `balloon`, `shares`, `cores`, `sockets`, `description`
- `hostpci*` entries: resolves desired PCI config, compares with current, drops unknown/stale entries, adds/updates as needed.

### Diff Display

For each changed property, show:
```
  memory: 4096 -> 65536
  hostpci0: (none) -> 0f:00.0,pcie=1,x-vga=1,rombar=1,romfile=llm-runner-pci-gpu.rom
  hostpci5: 03:00.0,pcie=1 -> (removed)
```

### Implementation

New file: `tools/ncf/ncf/commands/update_proxmox_vm.py`

Core logic:
1. Query NixOS config for desired state.
2. Find VM by hostname via Proxmox API (`client.vm_exists(hostname)`).
3. Check VM is stopped (fail with clear message if running).
4. Get current VM config via API.
5. Compute desired values for memory/cpu/pci properties.
6. Diff current vs desired.
7. If `--apply`: execute `qm set` commands. Delete stale `hostpci*` entries using `qm set <vmid> -delete hostpciN`.

### CLI Registration

In `cli.py`, register as `@machine_app.command("update-proxmox-vm")`.

## Files Modified

- `modules/qemu-guest.nix` — add `pci-passthrough` option + assertion
- `modules/machines/llm-runner.nix` — update pci-passthrough config (fix `GAT102` typo to `GA102`, add `radeon` mapped device, remove redundant default values)
- `tools/ncf/ncf/commands/provision_vm_anywhere.py` — call shared PCI passthrough step
- `tools/ncf/ncf/commands/pci_passthrough.py` — new shared helper module
- `tools/ncf/ncf/commands/update_proxmox_vm.py` — new subcommand implementation
- `tools/ncf/ncf/cli.py` — register new subcommand, import new module

## Shared Code

PCI resolution and ROM upload functions shared between provisioning and update, extracted to `tools/ncf/ncf/commands/pci_passthrough.py`:
- `resolve_pci_device(proxmox_host, device_id, all_functions) -> str`
- `upload_rom_file(proxmox_host, rom_path) -> str`
- `build_hostpci_spec(entry, proxmox_host) -> str`
- `configure_pci_passthrough(proxmox_host, vmid, pci_config, dry_run) -> list[tuple[str, str]]` — returns list of (key, value) pairs applied
