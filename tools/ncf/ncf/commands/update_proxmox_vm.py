"""Update Proxmox VM configuration to match NixOS config.

Compares the current VM configuration on Proxmox with the desired
configuration from NixOS, shows a diff, and optionally applies changes.
Dry-run by default.
"""

import shlex
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
        qm_cmd = ["qm", "set", str(vmid)] + set_args
        cmd = ["ssh", f"root@{proxmox_host}", shlex.join(qm_cmd)]
        subprocess.run(cmd, check=True)

    if delete_keys:
        qm_cmd = ["qm", "set", str(vmid), "-delete", ",".join(delete_keys)]
        cmd = ["ssh", f"root@{proxmox_host}", shlex.join(qm_cmd)]
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
