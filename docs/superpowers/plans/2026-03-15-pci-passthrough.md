# PCI Passthrough Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add PCI device passthrough support to the NixOS VM provisioning system, including a new subcommand to update existing VM configurations.

**Architecture:** Define `pci-passthrough` as an `attrsOf submodule` NixOS option in `qemu-guest.nix`. Extract PCI resolution/ROM upload logic to a shared helper `pci_passthrough.py`. Add a PCI passthrough step to `provision_vm_anywhere.py`. Create a new `update_proxmox_vm.py` subcommand for updating existing VMs (dry-run by default).

**Tech Stack:** Nix (NixOS module options), Python (typer CLI, subprocess for SSH/SCP), Proxmox `qm` CLI

**Spec:** `docs/superpowers/specs/2026-03-15-pci-passthrough-design.md`

**Important:** All ncf/pytest/python commands MUST be run through nix wrappers:
- Tests: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/... -v`
- ncf: `nix run .#env-aware-nix-run -- .#ncf -- [args]`
- Python: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command python -c "..."`

---

## File Structure

| File | Action | Responsibility |
|------|--------|---------------|
| `modules/qemu-guest.nix` | Modify | Add `pci-passthrough` option definition + assertion |
| `modules/machines/llm-runner.nix` | Modify | Update pci-passthrough config (fix typo, add radeon, remove redundant defaults) |
| `tools/ncf/ncf/commands/pci_passthrough.py` | Create | Shared helpers: resolve PCI device, upload ROM, build hostpci spec |
| `tools/ncf/ncf/commands/provision_vm_anywhere.py` | Modify | Add PCI passthrough step calling shared helpers |
| `tools/ncf/ncf/commands/update_proxmox_vm.py` | Create | New subcommand: diff and update existing VM config |
| `tools/ncf/ncf/proxmox_api.py` | Modify | Add `get_vm_status()` method |
| `tools/ncf/ncf/cli.py` | Modify | Import + register `update-proxmox-vm` subcommand |
| `tools/ncf/tests/test_pci_passthrough.py` | Create | Unit tests for PCI resolution, spec building, diff computation |

---

## Task 1: NixOS Option Definition

**Files:**
- Modify: `modules/qemu-guest.nix:19-255` (options block)
- Modify: `modules/qemu-guest.nix:263-316` (assertions block)

- [ ] **Step 1: Add pci-passthrough option to qemu-guest.nix**

Add after the `cloudInit` block (line 254), before the closing `};` of the options block at line 255:

```nix
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
```

- [ ] **Step 2: Add assertions for pci-passthrough**

In the assertions list, after the closing `]` at line 315, before `++ diskoBusAssertions;` at line 316, add:

```nix
          ++ (lib.concatLists (lib.mapAttrsToList (label: entry: [
            {
              assertion = (entry.id != null) != (entry.mapping != null);
              message = "pci-passthrough.${label}: exactly one of 'id' or 'mapping' must be set";
            }
          ]) proxmox.pci-passthrough))
```

- [ ] **Step 3: Verify Nix evaluation**

Run: `nix eval .#nixosConfigurations.llm-runner.config.nixos-config.qemu-guest.proxmox.pci-passthrough --json 2>&1 | head -5`

This will fail because llm-runner.nix has a ROM filename typo and missing radeon. That's expected — we fix llm-runner in the next task.

- [ ] **Step 4: Commit**

```bash
git add modules/qemu-guest.nix
git commit -m "feat(qemu-guest): add pci-passthrough option definition"
```

---

## Task 2: Update llm-runner.nix

**Files:**
- Modify: `modules/machines/llm-runner.nix:37-53`

- [ ] **Step 1: Update pci-passthrough config**

Replace the entire `pci-passthrough` block (lines 37-53) with:

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

Changes: fix `GAT102` → `GA102`, remove redundant default values from `nvme`, add `radeon` mapped device.

- [ ] **Step 2: Verify Nix evaluation**

Run: `nix eval .#nixosConfigurations.llm-runner.config.nixos-config.qemu-guest.proxmox.pci-passthrough --json | python3 -m json.tool`

Expected: JSON output with 4 entries (gpu, gpu-sound, nvme, radeon) with correct defaults filled in.

- [ ] **Step 3: Commit**

```bash
git add modules/machines/llm-runner.nix
git commit -m "feat(llm-runner): configure PCI passthrough devices"
```

---

## Task 3: Shared PCI Passthrough Helpers

**Files:**
- Create: `tools/ncf/ncf/commands/pci_passthrough.py`
- Create: `tools/ncf/tests/test_pci_passthrough.py`

- [ ] **Step 1: Write tests for PCI address parsing, spec building, and diff computation**

Create `tools/ncf/tests/test_pci_passthrough.py`:

```python
"""Tests for PCI passthrough helpers."""

import pytest

from ncf.commands.pci_passthrough import build_hostpci_spec, parse_lspci_output
from ncf.commands.update_proxmox_vm import compute_diff


class TestParseLspciOutput:
    """Tests for parsing lspci output to find PCI addresses."""

    SAMPLE_LSPCI = (
        "00:00.0 Host bridge: Advanced Micro Devices, Inc. [AMD] Starship/Matisse Root Complex\n"
        "0e:00.0 Non-Volatile memory controller: Samsung Electronics Co Ltd NVMe SSD Controller SM981/PM981/PM983\n"
        "0f:00.0 VGA compatible controller: NVIDIA Corporation GA102 [GeForce RTX 3090] (rev a1)\n"
        "0f:00.1 Audio device: NVIDIA Corporation GA102 High Definition Audio Controller (rev a1)\n"
    )

    def test_finds_unique_device(self):
        addr = parse_lspci_output(
            self.SAMPLE_LSPCI,
            "Samsung Electronics Co Ltd NVMe SSD Controller SM981/PM981/PM983",
            all_functions=False,
        )
        assert addr == "0e:00.0"

    def test_all_functions_strips_function(self):
        addr = parse_lspci_output(
            self.SAMPLE_LSPCI,
            "Samsung Electronics Co Ltd NVMe SSD Controller SM981/PM981/PM983",
            all_functions=True,
        )
        assert addr == "0e:00"

    def test_fails_on_no_match(self):
        with pytest.raises(RuntimeError, match="No PCI device found"):
            parse_lspci_output(self.SAMPLE_LSPCI, "Nonexistent Device", all_functions=False)

    def test_fails_on_multiple_matches(self):
        with pytest.raises(RuntimeError, match="Multiple PCI devices found"):
            parse_lspci_output(self.SAMPLE_LSPCI, "NVIDIA Corporation GA102", all_functions=False)


class TestBuildHostpciSpec:
    """Tests for building Proxmox hostpci spec strings."""

    def test_basic_id_mode(self):
        spec = build_hostpci_spec(
            address="0e:00.0",
            mapping=None,
            pcie=True,
            primary_gpu=False,
            rom_bar=True,
            romfile=None,
        )
        assert spec == "0e:00.0,pcie=1,x-vga=0,rombar=1"

    def test_mapping_mode(self):
        spec = build_hostpci_spec(
            address=None,
            mapping="rtx-3090",
            pcie=True,
            primary_gpu=False,
            rom_bar=True,
            romfile=None,
        )
        assert spec == "mapping=rtx-3090,pcie=1,x-vga=0,rombar=1"

    def test_primary_gpu_with_rom(self):
        spec = build_hostpci_spec(
            address="0f:00.0",
            mapping=None,
            pcie=True,
            primary_gpu=True,
            rom_bar=True,
            romfile="llm-runner-pci-gpu.rom",
        )
        assert spec == "0f:00.0,pcie=1,x-vga=1,rombar=1,romfile=llm-runner-pci-gpu.rom"

    def test_no_pcie_no_rombar(self):
        spec = build_hostpci_spec(
            address="0e:00.0",
            mapping=None,
            pcie=False,
            primary_gpu=False,
            rom_bar=False,
            romfile=None,
        )
        assert spec == "0e:00.0,pcie=0,x-vga=0,rombar=0"


class TestComputeDiff:
    """Tests for computing config diffs."""

    def test_no_changes(self):
        current = {"memory": 2048, "cores": 2}
        desired = {"memory": "2048", "cores": "2"}
        assert compute_diff(current, desired) == []

    def test_changed_value(self):
        current = {"memory": 2048, "cores": 2}
        desired = {"memory": "65536", "cores": "2"}
        changes = compute_diff(current, desired)
        assert changes == [("memory", "2048", "65536")]

    def test_new_key(self):
        current = {"memory": 2048}
        desired = {"memory": "2048", "hostpci0": "0f:00.0,pcie=1"}
        changes = compute_diff(current, desired)
        assert changes == [("hostpci0", "(none)", "0f:00.0,pcie=1")]

    def test_stale_hostpci_removed(self):
        current = {"memory": 2048, "hostpci0": "0f:00.0,pcie=1", "hostpci1": "0e:00.0,pcie=1"}
        desired = {"memory": "2048", "hostpci0": "0f:00.0,pcie=1"}
        changes = compute_diff(current, desired)
        assert changes == [("hostpci1", "0e:00.0,pcie=1", "(removed)")]

    def test_mixed_changes(self):
        current = {"memory": 2048, "hostpci0": "old,pcie=1", "hostpci5": "stale"}
        desired = {"memory": "65536", "hostpci0": "new,pcie=1"}
        changes = compute_diff(current, desired)
        assert ("memory", "2048", "65536") in changes
        assert ("hostpci0", "old,pcie=1", "new,pcie=1") in changes
        assert ("hostpci5", "stale", "(removed)") in changes
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/test_pci_passthrough.py -v`

Expected: ImportError — modules don't exist yet.

- [ ] **Step 3: Implement pci_passthrough.py**

Create `tools/ncf/ncf/commands/pci_passthrough.py`:

```python
"""Shared helpers for PCI passthrough configuration on Proxmox."""

import subprocess
from typing import Any

from ..output import console


def parse_lspci_output(lspci_output: str, device_id: str, all_functions: bool) -> str:
    """Parse lspci output to find PCI bus address for a device.

    Args:
        lspci_output: Raw output from `lspci` command.
        device_id: Substring to match in lspci description.
        all_functions: If True, strip .function suffix for multifunction passthrough.

    Returns:
        PCI address (e.g., "0f:00.0" or "0f:00" if all_functions).

    Raises:
        RuntimeError: If match count != 1.
    """
    matches = []
    for line in lspci_output.strip().splitlines():
        if device_id in line:
            address = line.split()[0]
            matches.append(address)

    if len(matches) == 0:
        raise RuntimeError(f"No PCI device found matching: {device_id}")
    if len(matches) > 1:
        addrs = ", ".join(matches)
        raise RuntimeError(
            f"Multiple PCI devices found matching '{device_id}': {addrs}. "
            f"Use a more specific id string."
        )

    address = matches[0]
    if all_functions:
        # Strip .function suffix: "0f:00.0" -> "0f:00"
        address = address.rsplit(".", 1)[0]
    return address


def resolve_pci_device(
    proxmox_host: str, device_id: str, all_functions: bool
) -> str:
    """Resolve lspci description to PCI bus address on a remote host.

    Runs `lspci` on the Proxmox host via SSH.

    Args:
        proxmox_host: Proxmox hostname (e.g., "valak"). root@ is prepended automatically.
        device_id: Substring to match in lspci output.
        all_functions: If True, return bus:slot without .function.

    Returns:
        PCI address string.
    """
    result = subprocess.run(
        ["ssh", f"root@{proxmox_host}", "lspci"],
        capture_output=True,
        text=True,
        check=True,
    )
    return parse_lspci_output(result.stdout, device_id, all_functions)


def upload_rom_file(proxmox_host: str, rom_path: str, dest_name: str) -> None:
    """Upload ROM file to /usr/share/kvm/ on Proxmox host via SCP.

    Args:
        proxmox_host: Proxmox hostname (e.g., "valak"). root@ is prepended automatically.
        rom_path: Local path to ROM file (typically a Nix store path).
        dest_name: Destination filename (e.g., "llm-runner-pci-gpu.rom").
    """
    dest = f"root@{proxmox_host}:/usr/share/kvm/{dest_name}"
    console.print(f"  Uploading ROM: {dest_name}")
    subprocess.run(["scp", rom_path, dest], check=True)


def build_hostpci_spec(
    address: str | None,
    mapping: str | None,
    pcie: bool,
    primary_gpu: bool,
    rom_bar: bool,
    romfile: str | None,
) -> str:
    """Build a Proxmox hostpci specification string.

    Args:
        address: PCI bus address (for id mode), e.g., "0f:00.0".
        mapping: Proxmox mapped device name (for mapping mode).
        pcie: Present as PCIe device.
        primary_gpu: Mark as primary GPU (x-vga=1).
        rom_bar: Enable ROM BAR.
        romfile: ROM filename (basename in /usr/share/kvm/).

    Returns:
        hostpci spec string, e.g., "0f:00.0,pcie=1,x-vga=1,rombar=1,romfile=foo.rom"
    """
    if mapping:
        parts = [f"mapping={mapping}"]
    else:
        parts = [address]

    parts.append(f"pcie={'1' if pcie else '0'}")
    parts.append(f"x-vga={'1' if primary_gpu else '0'}")
    parts.append(f"rombar={'1' if rom_bar else '0'}")

    if romfile:
        parts.append(f"romfile={romfile}")

    return ",".join(parts)


def configure_pci_passthrough(
    proxmox_host: str,
    vmid: int,
    hostname: str,
    pci_config: dict[str, Any],
    dry_run: bool,
) -> list[tuple[str, str]]:
    """Configure PCI passthrough devices for a VM.

    Resolves device IDs, uploads ROMs, and runs `qm set` for each device.

    Args:
        proxmox_host: Proxmox hostname (without user@ prefix).
        vmid: VM ID.
        hostname: VM hostname (for ROM file naming).
        pci_config: Dict from NixOS config pci-passthrough option.
        dry_run: If True, only show what would be done.

    Returns:
        List of (hostpci key, spec value) pairs that were configured.
    """
    if not pci_config:
        return []

    results = []
    sorted_labels = sorted(pci_config.keys())

    for idx, label in enumerate(sorted_labels):
        entry = pci_config[label]
        hostpci_key = f"hostpci{idx}"

        entry_id = entry.get("id")
        entry_mapping = entry.get("mapping")
        all_functions = entry.get("all-functions", False)
        pcie = entry.get("pci-express", True)
        primary_gpu = entry.get("primary-gpu", False)
        rom_bar = entry.get("rom-bar", True)
        rom_path = entry.get("rom")

        # Resolve address or use mapping
        address = None
        if entry_id:
            if dry_run:
                address = f"<resolve:{entry_id}>"
            else:
                address = resolve_pci_device(proxmox_host, entry_id, all_functions)
                console.print(f"  {label}: resolved to {address}")

        # Upload ROM if specified
        romfile = None
        if rom_path:
            romfile = f"{hostname}-pci-{label}.rom"
            if not dry_run:
                upload_rom_file(proxmox_host, rom_path, romfile)

        spec = build_hostpci_spec(
            address=address,
            mapping=entry_mapping,
            pcie=pcie,
            primary_gpu=primary_gpu,
            rom_bar=rom_bar,
            romfile=romfile,
        )

        if dry_run:
            console.print(f"  [yellow]Would set --{hostpci_key} {spec}[/yellow]")
        else:
            cmd = ["qm", "set", str(vmid), f"--{hostpci_key}", spec]
            ssh_cmd = ["ssh", f"root@{proxmox_host}"] + cmd
            subprocess.run(ssh_cmd, check=True)
            console.print(f"  [green]{hostpci_key}: {spec}[/green]")

        results.append((hostpci_key, spec))

    return results
```

- [ ] **Step 4: Run tests to verify they pass**

Note: The `compute_diff` tests will still fail (module not created yet). Run only pci_passthrough tests first:

Run: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/test_pci_passthrough.py::TestParseLspciOutput tools/ncf/tests/test_pci_passthrough.py::TestBuildHostpciSpec -v`

Expected: All 8 tests pass.

- [ ] **Step 5: Commit**

```bash
git add tools/ncf/ncf/commands/pci_passthrough.py tools/ncf/tests/test_pci_passthrough.py
git commit -m "feat(ncf): add PCI passthrough shared helpers with tests"
```

---

## Task 4: Integrate PCI Passthrough into Provisioning

**Files:**
- Modify: `tools/ncf/ncf/commands/provision_vm_anywhere.py` (import at top, new step after disk config)

- [ ] **Step 1: Add PCI passthrough step to provision_vm_anywhere.py**

Add import at the top (after line 24 `from . import iso_installer`):

```python
from .pci_passthrough import configure_pci_passthrough
```

Add new step after the disk configuration block (after line 278, before Step 5c cloud-init at line 280). Insert:

```python
    # Step 5c: Configure PCI passthrough devices
    pci_config = vm_config.get("pci-passthrough", {})
    if pci_config:
        console.print("\n[bold]Step 5c:[/bold] Configuring PCI passthrough")
        configure_pci_passthrough(
            proxmox_host=proxmox_host,
            vmid=vmid,
            hostname=hostname,
            pci_config=pci_config,
            dry_run=dry_run,
        )
```

Renumber the subsequent steps: old 5c → 5d, old 6 → 7, etc.

- [ ] **Step 2: Verify no import errors**

Run: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command python -c "from ncf.commands.provision_vm_anywhere import run; print('OK')"`

Expected: `OK`

- [ ] **Step 3: Commit**

```bash
git add tools/ncf/ncf/commands/provision_vm_anywhere.py
git commit -m "feat(provision): add PCI passthrough step to VM provisioning"
```

---

## Task 5: Add get_vm_status to ProxmoxClient

**Files:**
- Modify: `tools/ncf/ncf/proxmox_api.py:79-81` (after get_vm_config)

- [ ] **Step 1: Add get_vm_status method**

Add after `get_vm_config` (line 81), before `start_vm` (line 83):

```python
    def get_vm_status(self, vmid: int) -> str:
        """Get VM power status.

        Returns:
            Status string, e.g., 'running', 'stopped'.
        """
        status = self.api.nodes(self.node).qemu(vmid).status.current.get()
        return status["status"]
```

- [ ] **Step 2: Commit**

```bash
git add tools/ncf/ncf/proxmox_api.py
git commit -m "feat(proxmox-api): add get_vm_status method"
```

---

## Task 6: Implement update-proxmox-vm Subcommand

**Files:**
- Create: `tools/ncf/ncf/commands/update_proxmox_vm.py`
- Modify: `tools/ncf/ncf/cli.py:16-35` (imports) and after line 832 (command registration)

- [ ] **Step 1: Create update_proxmox_vm.py**

Create `tools/ncf/ncf/commands/update_proxmox_vm.py`:

```python
"""Update Proxmox VM configuration to match NixOS config.

Compares the current VM configuration on Proxmox with the desired
configuration from NixOS, shows a diff, and optionally applies changes.
Dry-run by default.
"""

import subprocess
from typing import Any

from rich.panel import Panel

from .. import config
from ..nix import NixRunner
from ..output import console
from ..proxmox_api import ProxmoxClient
from .pci_passthrough import (
    build_hostpci_spec,
    resolve_pci_device,
    upload_rom_file,
)
from .provision_vm import query_nixos_config


def compute_desired_config(
    vm_config: dict[str, Any],
    proxmox_host: str,
    hostname: str,
) -> dict[str, str]:
    """Compute desired Proxmox config values from NixOS config.

    Args:
        vm_config: NixOS qemu-guest.proxmox config dict.
        proxmox_host: Proxmox hostname for PCI device resolution.
        hostname: VM hostname for ROM file naming.

    Returns:
        Dict of Proxmox config keys to desired values.
    """
    desired: dict[str, str] = {}

    # CPU/memory properties
    desired["memory"] = str(vm_config["memory"])
    desired["cores"] = str(vm_config["cores"])
    desired["sockets"] = str(vm_config.get("sockets", 1))

    balloon = vm_config.get("balloon")
    if balloon is not None:
        desired["balloon"] = str(balloon)

    shares = vm_config.get("shares", 1000)
    if shares != 1000:
        desired["shares"] = str(shares)

    description = vm_config.get("description")
    if description:
        desired["description"] = description

    # PCI passthrough
    pci_config = vm_config.get("pci-passthrough", {})
    sorted_labels = sorted(pci_config.keys())

    for idx, label in enumerate(sorted_labels):
        entry = pci_config[label]
        entry_id = entry.get("id")
        entry_mapping = entry.get("mapping")
        all_functions = entry.get("all-functions", False)

        address = None
        if entry_id:
            address = resolve_pci_device(proxmox_host, entry_id, all_functions)

        romfile = None
        if entry.get("rom"):
            romfile = f"{hostname}-pci-{label}.rom"

        spec = build_hostpci_spec(
            address=address,
            mapping=entry_mapping,
            pcie=entry.get("pci-express", True),
            primary_gpu=entry.get("primary-gpu", False),
            rom_bar=entry.get("rom-bar", True),
            romfile=romfile,
        )
        desired[f"hostpci{idx}"] = spec

    return desired


def compute_diff(
    current: dict[str, Any],
    desired: dict[str, str],
) -> list[tuple[str, str, str]]:
    """Compute diff between current and desired config.

    Args:
        current: Current Proxmox VM config.
        desired: Desired config values.

    Returns:
        List of (key, current_value, desired_value) tuples.
        current_value is "(none)" for new keys.
        desired_value is "(removed)" for keys to delete.
    """
    changes = []

    # Check desired values against current
    for key, desired_val in sorted(desired.items()):
        current_val = str(current.get(key, ""))
        if current_val != desired_val:
            changes.append((key, current_val or "(none)", desired_val))

    # Check for stale hostpci entries in current config
    desired_hostpci_keys = {k for k in desired if k.startswith("hostpci")}
    for key in sorted(current.keys()):
        if key.startswith("hostpci") and key not in desired_hostpci_keys:
            changes.append((key, str(current[key]), "(removed)"))

    return changes


def apply_changes(
    proxmox_host: str,
    vmid: int,
    hostname: str,
    vm_config: dict[str, Any],
    changes: list[tuple[str, str, str]],
) -> None:
    """Apply configuration changes to a Proxmox VM.

    Args:
        proxmox_host: Proxmox hostname.
        vmid: VM ID.
        hostname: VM hostname for ROM file naming.
        vm_config: NixOS config for ROM path lookup.
        changes: List of (key, old_val, new_val) from compute_diff.
    """
    pci_config = vm_config.get("pci-passthrough", {})
    sorted_labels = sorted(pci_config.keys())

    # Upload ROM files first
    for idx, label in enumerate(sorted_labels):
        entry = pci_config[label]
        rom_path = entry.get("rom")
        if rom_path:
            romfile = f"{hostname}-pci-{label}.rom"
            upload_rom_file(proxmox_host, rom_path, romfile)

    # Build qm set command for updates
    set_args = []
    delete_keys = []
    for key, old_val, new_val in changes:
        if new_val == "(removed)":
            delete_keys.append(key)
        else:
            set_args.extend([f"--{key}", new_val])

    if set_args:
        cmd = ["ssh", f"root@{proxmox_host}", "qm", "set", str(vmid)] + set_args
        subprocess.run(cmd, check=True)

    if delete_keys:
        cmd = [
            "ssh",
            f"root@{proxmox_host}",
            "qm",
            "set",
            str(vmid),
            "-delete",
            ",".join(delete_keys),
        ]
        subprocess.run(cmd, check=True)


def run(
    machine: str,
    proxmox_host: str,
    apply: bool = False,
) -> None:
    """Update Proxmox VM configuration to match NixOS config.

    Args:
        machine: NixOS configuration name.
        proxmox_host: Proxmox hostname.
        apply: If True, apply changes. If False (default), dry-run only.
    """
    mode = "APPLY" if apply else "DRY RUN"
    console.print(
        Panel(f"Update VM config: [bold]{machine}[/bold] on {proxmox_host} [{mode}]")
    )

    repo_root = config.find_repo_root()
    runner = NixRunner(verbosity=1, repo_root=repo_root)

    # Get desired config from NixOS
    console.print("\n[bold]Step 1:[/bold] Reading NixOS config")
    hostname = query_nixos_config(runner, machine, "config.networking.hostName")
    vm_config = query_nixos_config(
        runner, machine, "config.nixos-config.qemu-guest.proxmox"
    )
    console.print(f"  Hostname: {hostname}")

    # Connect to Proxmox and find VM
    console.print("\n[bold]Step 2:[/bold] Finding VM on Proxmox")
    client = ProxmoxClient(proxmox_host)
    vmid = client.vm_exists(hostname)
    if vmid is None:
        raise RuntimeError(f"VM '{hostname}' not found on {proxmox_host}")
    console.print(f"  Found VM: {hostname} (VMID {vmid})")

    # Check VM is stopped
    status = client.get_vm_status(vmid)
    if status != "stopped":
        raise RuntimeError(
            f"VM '{hostname}' is {status}. Stop it before updating config."
        )
    console.print(f"  Status: {status}")

    # Get current config
    console.print("\n[bold]Step 3:[/bold] Resolving desired config")
    current_config = client.get_vm_config(vmid)

    desired = compute_desired_config(vm_config, proxmox_host, hostname)

    # Compute and show diff
    console.print("\n[bold]Step 4:[/bold] Configuration diff")
    changes = compute_diff(current_config, desired)

    if not changes:
        console.print("  [green]No changes needed[/green]")
        return

    for key, old_val, new_val in changes:
        if new_val == "(removed)":
            console.print(f"  [red]{key}: {old_val} -> (removed)[/red]")
        elif old_val == "(none)":
            console.print(f"  [green]{key}: (none) -> {new_val}[/green]")
        else:
            console.print(f"  [yellow]{key}: {old_val} -> {new_val}[/yellow]")

    if not apply:
        console.print(
            "\n[yellow]Dry run — no changes applied. Use --apply to apply.[/yellow]"
        )
        return

    # Apply changes
    console.print("\n[bold]Step 5:[/bold] Applying changes")
    apply_changes(proxmox_host, vmid, hostname, vm_config, changes)
    console.print("[bold green]Done! Changes applied.[/bold green]")
```

- [ ] **Step 2: Register subcommand in cli.py**

Add import in `cli.py` (after line 31 `provision_vm_anywhere,`):

```python
    update_proxmox_vm,
```

Add command registration after line 832 (after `provision-vm-anywhere` command block):

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
):
    """Update Proxmox VM config to match NixOS config.

    Compares the current VM configuration on Proxmox with the desired
    configuration from NixOS and shows a diff. By default, operates in
    dry-run mode. Use --apply to actually make changes.

    The VM must be stopped before updating. Handles:
    - CPU/memory: cores, sockets, memory, balloon, shares
    - PCI passthrough: resolves devices, uploads ROMs, removes stale entries
    """
    update_proxmox_vm.run(
        machine=machine,
        proxmox_host=proxmox_host,
        apply=apply,
    )
```

- [ ] **Step 3: Run all tests including compute_diff tests**

Run: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/test_pci_passthrough.py -v`

Expected: All 13 tests pass (8 from Task 3 + 5 compute_diff tests).

- [ ] **Step 4: Verify CLI registration**

Run: `nix run .#env-aware-nix-run -- .#ncf -- machine update-proxmox-vm --help`

Expected: Help output showing machine argument, --proxmox-host, and --apply options.

- [ ] **Step 5: Commit**

```bash
git add tools/ncf/ncf/commands/update_proxmox_vm.py tools/ncf/ncf/cli.py
git commit -m "feat(ncf): add update-proxmox-vm subcommand"
```

---

## Task 7: End-to-End Smoke Test

- [ ] **Step 1: Run all tests**

Run: `nix run .#env-aware-nix-develop -- .#devShells.x86_64-linux.default --command pytest tools/ncf/tests/ -v`

Expected: All tests pass.

- [ ] **Step 2: Test dry-run provisioning**

Run: `nix run .#env-aware-nix-run -- .#ncf -- machine provision-vm-anywhere llm-runner -p valak --dry-run`

Expected: Output shows PCI passthrough step with `Would set --hostpci0 ...` through `--hostpci3`.

- [ ] **Step 3: Test update-proxmox-vm dry-run against valak**

If llm-runner VM exists on valak (or create a test VM):

Run: `nix run .#env-aware-nix-run -- .#ncf -- machine update-proxmox-vm llm-runner -p valak`

Expected: Shows config diff in dry-run mode.

- [ ] **Step 4: Commit any fixes needed**

If fixes were needed during smoke testing, commit them.
