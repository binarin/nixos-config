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
