# Post-Install Cleanup Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend `update-proxmox-vm` to clean up installation artefacts (ISO, cloud-init) and manage boot order.

**Architecture:** Add `bootable` option to PCI passthrough in qemu-guest.nix. Extend `compute_desired_config()` to compute boot order from NixOS config and optionally add IDE device deletions. Reuse existing `(removed)` sentinel and `--delete` pattern in `apply_changes()`.

**Tech Stack:** Python (typer CLI), Nix (NixOS modules), Proxmox `qm` CLI via SSH

**Testing:** Run tests via `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/ -v`

**Spec:** `docs/superpowers/specs/2026-03-15-post-install-cleanup-design.md`

---

## Chunk 1: Boot order computation

### Task 1: Add `bootable` option to PCI passthrough in qemu-guest.nix

**Files:**
- Modify: `modules/qemu-guest.nix:256-306` (pci-passthrough submodule options)

- [ ] **Step 1: Add `bootable` option to pci-passthrough submodule**

In `modules/qemu-guest.nix`, add inside the `pci-passthrough` submodule `options` block (after the `rom` option, before the closing `};` at line 301):

```nix
                bootable = lib.mkOption {
                  type = lib.types.bool;
                  default = false;
                  description = "Mark this PCI device as the boot device.";
                };
```

- [ ] **Step 2: Add `bootable = true` to llm-runner NVMe passthrough**

In `modules/machines/llm-runner.nix`, the `nvme` PCI passthrough entry (line 49-52) currently has `id` and `bootable = true`. Verify it already has `bootable = true` — if not, add it:

```nix
          nvme = {
            id = "Samsung Electronics Co Ltd NVMe SSD Controller SM981/PM981/PM983";
            bootable = true;
          };
```

- [ ] **Step 3: Commit**

```bash
git add modules/qemu-guest.nix modules/machines/llm-runner.nix
git commit -m "feat(qemu-guest): add bootable option to PCI passthrough"
```

### Task 2: Add boot order tests

**Files:**
- Modify: `tools/ncf/tests/test_pci_passthrough.py` (add new test class)
- Modify: `tools/ncf/ncf/commands/update_proxmox_vm.py` (will be modified in Task 3)

- [ ] **Step 1: Write failing tests for boot order computation**

Add to `tools/ncf/tests/test_pci_passthrough.py`:

```python
from ncf.commands.update_proxmox_vm import compute_boot_order


class TestComputeBootOrder:
    """Tests for computing boot order from NixOS config."""

    def test_pci_device_bootable(self):
        """A PCI passthrough device marked bootable produces correct boot order."""
        vm_config = {
            "pci-passthrough": {
                "gpu-sound": {"bootable": False},
                "nvme": {"bootable": True},
            },
            "disks": [],
        }
        # PCI devices are sorted alphabetically: gpu-sound=hostpci0, nvme=hostpci1
        assert compute_boot_order(vm_config) == "order=hostpci1"

    def test_disk_bootable(self):
        """A disk with bootOrder set produces correct boot order."""
        vm_config = {
            "pci-passthrough": {},
            "disks": [
                {"bus": "scsi", "index": 0, "bootOrder": 1},
            ],
        }
        assert compute_boot_order(vm_config) == "order=scsi0"

    def test_no_bootable_device_returns_none(self):
        """When no device is marked bootable, returns None."""
        vm_config = {
            "pci-passthrough": {"nvme": {"bootable": False}},
            "disks": [{"bus": "scsi", "index": 0, "bootOrder": None}],
        }
        assert compute_boot_order(vm_config) is None

    def test_multiple_bootable_devices_raises(self):
        """More than one bootable device raises RuntimeError."""
        vm_config = {
            "pci-passthrough": {"nvme": {"bootable": True}},
            "disks": [{"bus": "scsi", "index": 0, "bootOrder": 1}],
        }
        with pytest.raises(RuntimeError, match="exactly 1 bootable device"):
            compute_boot_order(vm_config)

    def test_multiple_pci_bootable_raises(self):
        """Two PCI devices marked bootable raises RuntimeError."""
        vm_config = {
            "pci-passthrough": {
                "nvme1": {"bootable": True},
                "nvme2": {"bootable": True},
            },
            "disks": [],
        }
        with pytest.raises(RuntimeError, match="exactly 1 bootable device"):
            compute_boot_order(vm_config)
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/test_pci_passthrough.py::TestComputeBootOrder -v`

Expected: ImportError — `compute_boot_order` does not exist yet.

- [ ] **Step 3: Commit failing tests**

```bash
git add tools/ncf/tests/test_pci_passthrough.py
git commit -m "test: add failing tests for boot order computation"
```

### Task 3: Implement boot order computation

**Files:**
- Modify: `tools/ncf/ncf/commands/update_proxmox_vm.py:26-88` (compute_desired_config) — add `compute_boot_order` function and integrate it

- [ ] **Step 1: Implement `compute_boot_order` function**

Add to `tools/ncf/ncf/commands/update_proxmox_vm.py` (before `compute_desired_config`):

```python
def compute_boot_order(vm_config: dict[str, Any]) -> str | None:
    """Compute Proxmox boot order string from NixOS config.

    Returns:
        Boot order string (e.g. "order=hostpci2") or None if no device is bootable.

    Raises:
        RuntimeError: If more than one device is marked bootable.
    """
    bootable_keys: list[str] = []

    # Check disks with bootOrder set
    for disk in vm_config.get("disks", []):
        if disk.get("bootOrder") is not None:
            key = f"{disk['bus']}{disk['index']}"
            bootable_keys.append(key)

    # Check PCI passthrough devices with bootable=True
    pci_config = vm_config.get("pci-passthrough", {})
    sorted_labels = sorted(pci_config.keys())
    for idx, label in enumerate(sorted_labels):
        if pci_config[label].get("bootable", False):
            bootable_keys.append(f"hostpci{idx}")

    if len(bootable_keys) == 0:
        return None
    if len(bootable_keys) > 1:
        raise RuntimeError(
            f"Expected exactly 1 bootable device, found {len(bootable_keys)}: {bootable_keys}"
        )

    return f"order={bootable_keys[0]}"
```

- [ ] **Step 2: Run tests to verify they pass**

Run: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/test_pci_passthrough.py::TestComputeBootOrder -v`

Expected: All 5 tests PASS.

- [ ] **Step 3: Integrate boot order into `compute_desired_config`**

In `compute_desired_config()`, add at the end (before `return desired`, line 88):

```python
    # Boot order
    boot_order = compute_boot_order(vm_config)
    if boot_order is not None:
        desired["boot"] = boot_order
```

- [ ] **Step 4: Run all tests**

Run: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/ -v`

Expected: All tests PASS.

- [ ] **Step 5: Commit**

```bash
git add tools/ncf/ncf/commands/update_proxmox_vm.py
git commit -m "feat(update-proxmox-vm): compute boot order from NixOS config"
```

## Chunk 2: Cleanup flag and IDE deletion

### Task 4: Add cleanup tests

**Files:**
- Modify: `tools/ncf/tests/test_pci_passthrough.py` (add cleanup diff tests)

- [ ] **Step 1: Write failing tests for cleanup diff behavior**

Add to `tools/ncf/tests/test_pci_passthrough.py`:

```python
from ncf.commands.update_proxmox_vm import add_cleanup_entries


class TestAddCleanupEntries:
    """Tests for adding IDE cleanup entries to desired config."""

    def test_adds_ide_deletions_when_present(self):
        """When ide0 and ide2 exist in current config, adds removal entries."""
        current = {
            "ide0": "local-zfs:vm-125-cloudinit,media=cdrom",
            "ide2": "local:iso/nixos-installer.iso,media=cdrom,size=4563216K",
            "memory": "2048",
        }
        desired = {"memory": "2048"}
        add_cleanup_entries(current, desired)
        assert desired["ide0"] == "(removed)"
        assert desired["ide2"] == "(removed)"

    def test_skips_missing_ide_devices(self):
        """When ide0/ide2 are absent, does not add removal entries."""
        current = {"memory": "2048"}
        desired = {"memory": "2048"}
        add_cleanup_entries(current, desired)
        assert "ide0" not in desired
        assert "ide2" not in desired

    def test_partial_cleanup(self):
        """When only ide2 exists, only ide2 gets removal entry."""
        current = {
            "ide2": "local:iso/nixos-installer.iso,media=cdrom",
            "memory": "2048",
        }
        desired = {"memory": "2048"}
        add_cleanup_entries(current, desired)
        assert "ide0" not in desired
        assert desired["ide2"] == "(removed)"
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/test_pci_passthrough.py::TestAddCleanupEntries -v`

Expected: ImportError — `add_cleanup_entries` does not exist.

- [ ] **Step 3: Commit failing tests**

```bash
git add tools/ncf/tests/test_pci_passthrough.py
git commit -m "test: add failing tests for cleanup entry generation"
```

### Task 5: Implement cleanup entries and CLI flag

**Files:**
- Modify: `tools/ncf/ncf/commands/update_proxmox_vm.py:88-245` (add `add_cleanup_entries`, modify `run()`)
- Modify: `tools/ncf/ncf/cli.py:833-857` (add `--cleanup-install` flag)

- [ ] **Step 1: Implement `add_cleanup_entries` function**

Add to `tools/ncf/ncf/commands/update_proxmox_vm.py` (after `compute_boot_order`, before `compute_desired_config`):

```python
def add_cleanup_entries(
    current: dict[str, Any],
    desired: dict[str, str],
) -> None:
    """Add IDE device removal entries to desired config for post-install cleanup.

    Marks ide0 (cloud-init) and ide2 (installer ISO) for deletion,
    but only if they exist in the current VM config.
    """
    for key in ("ide0", "ide2"):
        if key in current:
            desired[key] = "(removed)"
```

- [ ] **Step 2: Run cleanup tests to verify they pass**

Run: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/test_pci_passthrough.py::TestAddCleanupEntries -v`

Expected: All 3 tests PASS.

- [ ] **Step 3: Update `compute_diff` to handle IDE removals**

In `compute_diff()` (line 114-120), extend the stale-key detection to also catch IDE keys marked `(removed)`. The current code only catches stale `hostpci` keys. Since `add_cleanup_entries` puts `(removed)` directly into `desired`, the existing diff logic at lines 108-112 already handles it — `desired["ide0"] = "(removed)"` will be compared to `current["ide0"]` and show as a change. No modification needed to `compute_diff`.

However, verify this by checking: the `apply_changes` function at line 154 already handles `new_val == "(removed)"` by adding to `delete_keys`. So the full pipeline works:
1. `add_cleanup_entries` sets `desired["ide0"] = "(removed)"`
2. `compute_diff` sees `current["ide0"] != "(removed)"`, adds change
3. `apply_changes` sees `new_val == "(removed)"`, adds to `--delete`

No code change needed — mark as verified.

- [ ] **Step 4: Add `cleanup_install` parameter to `run()`**

Modify the `run()` function signature in `update_proxmox_vm.py` (line 170-174):

```python
def run(
    machine: str,
    proxmox_host: str,
    apply: bool = False,
    cleanup_install: bool = False,
) -> None:
```

After `desired = compute_desired_config(vm_config, proxmox_host, hostname)` (line 218), add:

```python
    if cleanup_install:
        add_cleanup_entries(current_config, desired)
```

- [ ] **Step 5: Add `--cleanup-install` flag to CLI**

In `tools/ncf/ncf/cli.py`, modify the `machine_update_proxmox_vm_cmd` function (lines 833-857). Add the parameter and pass it through:

```python
@machine_app.command("update-proxmox-vm")
def machine_update_proxmox_vm_cmd(
    machine: str = typer.Argument(help="Machine name to update"),
    proxmox_host: str = typer.Option(
        ..., "--proxmox-host", "-p", help="Proxmox host where the VM runs"
    ),
    apply: bool = typer.Option(
        False, "--apply", help="Apply changes (default is dry-run)"
    ),
    cleanup_install: bool = typer.Option(
        False, "--cleanup-install", help="Remove installation artefacts (ISO, cloud-init)"
    ),
):
    """Update Proxmox VM config to match NixOS config.

    Compares the current VM configuration on Proxmox with the desired
    configuration from NixOS and shows a diff. By default, operates in
    dry-run mode. Use --apply to actually make changes.

    The VM must be stopped before updating. Handles:
    - CPU/memory: cores, sockets, memory, balloon, shares
    - PCI passthrough: resolves devices, uploads ROMs, removes stale entries
    - Boot order: computed from bootable disk/PCI device
    - Post-install cleanup (--cleanup-install): removes ISO and cloud-init drives
    """
    update_proxmox_vm.run(
        machine=machine,
        proxmox_host=proxmox_host,
        apply=apply,
        cleanup_install=cleanup_install,
    )
```

- [ ] **Step 6: Run all tests**

Run: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/ -v`

Expected: All tests PASS.

- [ ] **Step 7: Commit**

```bash
git add tools/ncf/ncf/commands/update_proxmox_vm.py tools/ncf/ncf/cli.py
git commit -m "feat(update-proxmox-vm): add --cleanup-install flag for post-install cleanup"
```

## Chunk 3: Manual verification on llm-runner

### Task 6: Dry-run verification on llm-runner

**Files:** None (manual verification only)

- [ ] **Step 1: Run dry-run without cleanup flag**

```bash
nix run .#env-aware-nix-run -- .#ncf -- machine update-proxmox-vm llm-runner --proxmox-host valak
```

Expected: Shows boot order diff (`boot: order=ide2;... -> order=hostpci1` or similar). No IDE deletion entries shown.

- [ ] **Step 2: Run dry-run with cleanup flag**

```bash
nix run .#env-aware-nix-run -- .#ncf -- machine update-proxmox-vm llm-runner --proxmox-host valak --cleanup-install
```

Expected: Shows boot order diff plus `ide0` and `ide2` deletion entries. No changes applied.

- [ ] **Step 3: Verify boot order key mapping is correct**

Check that the bootable NVMe device (`nvme` label) maps to the correct `hostpciN` index. PCI devices are sorted alphabetically: `gpu-sound` (hostpci0), `nvme` (hostpci1). But the current Proxmox config shows `hostpci2` for the NVMe — this is because the commented-out `gpu` entry still occupies a slot at eval time... or it doesn't since it's commented out.

Verify by reading the dry-run output. The sorted labels from NixOS config are: `gpu-sound`, `nvme` → hostpci0, hostpci1. Current Proxmox has hostpci0 (gpu), hostpci1 (gpu-sound), hostpci2 (nvme). The diff should show stale hostpci entries being removed and new mapping applied.

- [ ] **Step 4: Apply changes if diff looks correct**

```bash
nix run .#env-aware-nix-run -- .#ncf -- machine update-proxmox-vm llm-runner --proxmox-host valak --cleanup-install --apply
```

- [ ] **Step 5: Verify on Proxmox**

```bash
ssh root@valak "qm config $(qm list | grep llm-runner | awk '{print $1}')"
```

Expected: No `ide0` or `ide2` entries. `boot: order=hostpci1` (or correct index). VM should still boot correctly from NVMe.
