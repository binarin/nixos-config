# Post-Install Cleanup for update-proxmox-vm

## Problem

After VM provisioning via `provision_vm_anywhere`, installation artefacts remain attached to the VM:
- Installer ISO on `ide2` (cdrom)
- Cloud-init drive on `ide0`
- Boot order includes installation media

These should be cleaned up after NixOS installation is complete.

## Design

### Approach

Extend the existing diff-based `update-proxmox-vm` command (Approach A). Cleanup actions appear as diff entries alongside CPU/memory/PCI changes, following the existing dry-run/apply pattern.

### Changes

#### 1. NixOS module: `qemu-guest.nix`

Add `bootable` boolean option (default `false`) to the `pci-passthrough` submodule. This complements the existing `bootOrder` option on disks.

Ways to mark a device as bootable:
- **Disk:** set `bootOrder` to a non-null integer
- **PCI passthrough device:** set `bootable = true`

#### 2. Boot order computation: `update_proxmox_vm.py`

`compute_desired_config()` computes boot order when any device is marked bootable:

1. Collect disks with `bootOrder != null` → map to Proxmox key (e.g. `scsi0`)
2. Collect PCI passthrough devices with `bootable == true` → map to Proxmox key (e.g. `hostpci2`)
3. If no devices are marked bootable, skip boot order computation entirely (backwards-compatible with existing VMs)
4. If exactly 1 device is bootable, add `boot: order=<key>` to desired config
5. If more than 1 device is bootable, error out

Boot order drift is visible and correctable whenever a bootable device is configured.

#### 3. Cleanup actions: `update_proxmox_vm.py`

When `--cleanup-install` is passed, additional delete entries are added to the diff for IDE devices that are present in the current VM config:
- `ide2: <delete>` — fully remove installer ISO/cdrom device
- `ide0: <delete>` — fully remove cloud-init drive

If `ide0` or `ide2` are not present in the current config (already cleaned up), the corresponding delete entry is silently skipped — no diff shown, no action taken. This makes the operation idempotent.

Deletions use `qm set <vmid> --delete ide0,ide2` which fully removes the devices (not the same as `detach_iso()` which keeps an empty cdrom slot). The existing `ProxmoxClient.detach_iso()` and `set_boot_order()` methods are not reused — `update_proxmox_vm` consistently uses raw `qm set` via SSH, and this follows that pattern.

`apply_changes()` is extended to handle a "delete" sentinel value. Delete keys are collected and applied via a single `qm set --delete` call.

These actions only appear in the diff when `--cleanup-install` is specified. Changes are only applied when both `--cleanup-install` and `--apply` flags are given.

#### 4. Provisioning: `provision_vm_anywhere.py`

No change — boot order stays as `order=ide2;<boot_disk_key>`, allowing the VM to fall through to disk after install and reboot.

#### 5. CLI: `cli.py`

Add `--cleanup-install` boolean flag (default `False`) to the `update-proxmox-vm` command, passed through to `update_proxmox_vm.run()`.

### Workflow

1. `ncf machine provision-vm-anywhere llm-runner --proxmox-host valak` — creates VM, boots from ISO with disk fallback
2. Install NixOS (via clan or manually), VM reboots into installed system
3. `ncf machine update-proxmox-vm llm-runner --proxmox-host valak --cleanup-install` — shows diff (dry-run)
4. `ncf machine update-proxmox-vm llm-runner --proxmox-host valak --cleanup-install --apply` — applies cleanup + boot order + any other config drift

### Example diff output

```
boot: order=ide2;scsi0 -> order=hostpci2
ide0: local-zfs:vm-125-cloudinit,media=cdrom -> <delete>
ide2: local:iso/nixos-installer.iso,media=cdrom -> <delete>
```

### Files modified

- `modules/qemu-guest.nix` — add `bootable` option to pci-passthrough submodule
- `tools/ncf/ncf/commands/update_proxmox_vm.py` — boot order computation, cleanup actions, deletion support in apply
- `tools/ncf/ncf/cli.py` — add `--cleanup-install` flag
