"""Provision Proxmox VMs for clan install.

This module creates and configures VMs on Proxmox with cloud-init IP
assignment, ready for the user to run `clan machines install` separately.

Workflow:
1. Create VM with metadata from NixOS config
2. Configure cloud-init for static IP assignment
3. Boot from installer ISO
4. Verify VM gets the expected IP
"""

import json
import shlex
import socket
import subprocess
import time
from typing import Any, Optional

from rich.panel import Panel

from .. import config
from ..nix import NixRunner
from ..proxmox_api import ProxmoxClient
from . import iso_installer
from .pci_passthrough import configure_pci_passthrough
from .provision_vm import (
    build_qm_create_command,
    query_nixos_config,
    validate_existing_vm,
)

from ..output import console


def wait_for_ssh(
    host: str,
    port: int = 22,
    timeout: int = 300,
    interval: int = 5,
) -> bool:
    """Wait for SSH to become available.

    Args:
        host: Hostname or IP address
        port: SSH port
        timeout: Maximum time to wait in seconds
        interval: Time between checks in seconds

    Returns:
        True if SSH is reachable, False if timeout
    """
    start_time = time.time()
    while time.time() - start_time < timeout:
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(5)
            result = sock.connect_ex((host, port))
            sock.close()
            if result == 0:
                return True
        except (socket.error, socket.timeout):
            pass
        time.sleep(interval)
    return False


def run(
    machine: str,
    proxmox_host: str,
    bridge: str = "vmbr0",
    ssh_timeout: int = 300,
    force_rebuild_iso: bool = False,
    dry_run: bool = False,
) -> None:
    """Provision a Proxmox VM for clan install.

    Creates a VM with cloud-init IP configuration, boots from installer
    ISO, and verifies the VM gets its expected IP address. The VM is then
    ready for `clan machines install`.

    Args:
        machine: NixOS configuration name
        proxmox_host: Proxmox host to provision on
        bridge: Network bridge name
        ssh_timeout: Timeout for SSH connectivity in seconds
        force_rebuild_iso: Force rebuild and re-upload the installer ISO
        dry_run: Show what would be done
    """
    console.print(Panel(f"Provisioning VM: [bold]{machine}[/bold] on {proxmox_host}"))

    repo_root = config.find_repo_root()
    runner = NixRunner(verbosity=1, repo_root=repo_root)

    # Initialize Proxmox client
    client: Optional[ProxmoxClient] = None
    if not dry_run:
        console.print(f"\nConnecting to Proxmox host: {proxmox_host}")
        client = ProxmoxClient(proxmox_host)
        console.print(f"  Connected to node: {client.node}")

    # Step 1: Gather metadata from NixOS config
    console.print("\n[bold]Step 1:[/bold] Gathering metadata from NixOS config")

    hostname = query_nixos_config(runner, machine, "config.networking.hostName")
    console.print(f"  Hostname: {hostname}")

    vm_config = query_nixos_config(
        runner, machine, "config.nixos-config.qemu-guest.proxmox"
    )
    console.print(f"  Memory: {vm_config['memory']} MB")
    console.print(f"  Cores: {vm_config['cores']}")
    console.print(f"  BIOS: {vm_config.get('bios', 'seabios')}")

    # Get network info
    ip_alloc = query_nixos_config(
        runner, machine, "config.inventory.hostIpAllocation.home.primary"
    )
    console.print(f"  IP: {ip_alloc['address']}")

    network_info = query_nixos_config(runner, machine, "config.inventory.networks.home")
    network_config = {
        "address": ip_alloc["address"],
        "prefix": network_info["prefix"],
        "gateway": network_info["gateway"],
        "mac": ip_alloc.get("mac"),
    }

    # Override bridge from command line
    vm_config["network"] = vm_config.get("network", {})
    vm_config["network"]["bridge"] = bridge

    # Step 2: Check if VM exists
    console.print("\n[bold]Step 2:[/bold] Checking for existing VM")

    if dry_run:
        console.print(f"  [yellow]Would check for VM '{hostname}'[/yellow]")
        existing_vmid = None
    else:
        assert client is not None
        existing_vmid = client.vm_exists(hostname)

    if existing_vmid:
        console.print(
            f"  [yellow]VM '{hostname}' exists with VMID {existing_vmid}[/yellow]"
        )
        console.print("\n[bold]Validation mode:[/bold] Comparing configuration")

        if not dry_run:
            assert client is not None
            current_config = client.get_vm_config(existing_vmid)
            mismatches = validate_existing_vm(current_config, hostname, vm_config)

            if mismatches:
                console.print("\n[red]Configuration mismatches found:[/red]")
                for m in mismatches:
                    console.print(f"  - {m}")
            else:
                console.print("[green]Configuration matches expected values[/green]")

        console.print(
            "\n[yellow]Existing VM not modified. "
            "Delete it manually if you want to reprovision.[/yellow]"
        )
        return

    console.print(f"  [green]VM '{hostname}' does not exist[/green]")

    # Step 3: Ensure installer ISO is available
    console.print("\n[bold]Step 3:[/bold] Ensuring installer ISO is available")

    if dry_run:
        console.print(f"  [yellow]Would check/upload ISO to {proxmox_host}[/yellow]")
        iso_ref = "local:iso/nixos-installer.iso"
    else:
        iso_ref = iso_installer.ensure_iso_on_proxmox(
            proxmox_host=proxmox_host,
            storage="local",
            build_if_missing=True,
            force=force_rebuild_iso,
            verbosity=1,
        )

    # Step 4: Create VM
    console.print("\n[bold]Step 4:[/bold] Creating VM")

    if dry_run:
        vmid = 999
        console.print("  [yellow]Would get next VMID from Proxmox[/yellow]")
    else:
        assert client is not None
        vmid = client.get_next_vmid()
        console.print(f"  Next available VMID: {vmid}")

    qm_cmd = build_qm_create_command(
        vmid=vmid,
        hostname=hostname,
        vm_config=vm_config,
        network_config=network_config,
    )

    if dry_run:
        console.print(f"  [yellow]Would run on {proxmox_host}:[/yellow]")
        console.print(f"    {' '.join(qm_cmd)}")
    else:
        console.print(f"  Creating VM {vmid}...")
        ssh_cmd = ["ssh", f"root@{proxmox_host}", shlex.join(qm_cmd)]
        subprocess.run(ssh_cmd, check=True)
        console.print(f"  [green]VM {vmid} created[/green]")

    # Step 5a: Configure EFI disk if UEFI
    if vm_config.get("bios") == "ovmf":
        console.print("\n[bold]Step 5a:[/bold] Configuring EFI disk")
        efidisk = vm_config.get("efidisk", {})
        storage = efidisk.get("storage", "local-zfs")
        efitype = efidisk.get("efitype", "4m")
        secure_boot = efidisk.get("secureBoot", False)
        pre_enrolled = "1" if secure_boot else "0"
        efi_spec = f"{storage}:1,efitype={efitype},pre-enrolled-keys={pre_enrolled}"

        efi_cmd = ["qm", "set", str(vmid), "--efidisk0", efi_spec]

        if dry_run:
            console.print(f"  [yellow]Would run: {' '.join(efi_cmd)}[/yellow]")
        else:
            ssh_cmd = ["ssh", f"root@{proxmox_host}", shlex.join(efi_cmd)]
            subprocess.run(ssh_cmd, check=True)
            console.print(f"  [green]EFI disk configured[/green]")

    # Step 5b: Configure disks from NixOS config
    console.print("\n[bold]Step 5b:[/bold] Configuring disks")
    disks = vm_config.get("disks", [])
    boot_disk_key = "scsi0"  # default
    if not disks:
        # Default disk if none specified
        console.print("  No disks specified, creating default 32G disk")
        disk_cmd = ["qm", "set", str(vmid), "--scsi0", "local-zfs:32"]
        if dry_run:
            console.print(f"  [yellow]Would run: {' '.join(disk_cmd)}[/yellow]")
        else:
            ssh_cmd = ["ssh", f"root@{proxmox_host}", shlex.join(disk_cmd)]
            subprocess.run(ssh_cmd, check=True)
    else:
        for disk in disks:
            disk_type = disk.get("type")
            bus = disk.get("bus", "scsi")
            index = disk.get("index", 0)
            disk_key = f"{bus}{index}"

            # Track first disk as boot disk
            if boot_disk_key == "scsi0" or index == 0:
                boot_disk_key = disk_key

            if disk_type == "passthrough":
                device = disk.get("device")
                disk_cmd = ["qm", "set", str(vmid), f"--{disk_key}", device]
            elif disk_type == "image":
                storage = disk.get("storage", "local-zfs")
                size = disk.get("size", "32G")
                size_num = "".join(c for c in size if c.isdigit())
                disk_cmd = [
                    "qm",
                    "set",
                    str(vmid),
                    f"--{disk_key}",
                    f"{storage}:{size_num}",
                ]
            else:
                continue

            if dry_run:
                console.print(f"  [yellow]Would run: {' '.join(disk_cmd)}[/yellow]")
            else:
                ssh_cmd = ["ssh", f"root@{proxmox_host}"] + disk_cmd
                subprocess.run(ssh_cmd, check=True)
                console.print(f"  [green]Disk {disk_key} configured[/green]")

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

    # Step 5d: Attach cloud-init drive
    console.print("\n[bold]Step 5d:[/bold] Attaching cloud-init drive")
    cloud_init_config = vm_config.get("cloudInit", {})
    ci_storage = cloud_init_config.get("storage", "local-zfs")

    if dry_run:
        console.print(
            f"  [yellow]Would attach cloud-init drive on {ci_storage}[/yellow]"
        )
    else:
        ci_cmd = ["qm", "set", str(vmid), "--ide0", f"{ci_storage}:cloudinit"]
        ssh_cmd = ["ssh", f"root@{proxmox_host}", shlex.join(ci_cmd)]
        subprocess.run(ssh_cmd, check=True)
        console.print(f"  [green]Cloud-init drive attached[/green]")

    # Step 6: Attach ISO and set boot order
    console.print("\n[bold]Step 6:[/bold] Attaching ISO and configuring boot")

    if dry_run:
        console.print(f"  [yellow]Would attach ISO: {iso_ref}[/yellow]")
        console.print(f"  [yellow]Would set boot order: ide2;{boot_disk_key}[/yellow]")
    else:
        assert client is not None
        client.attach_iso(vmid, iso_ref)
        client.set_boot_order(vmid, f"order=ide2;{boot_disk_key}")
        console.print(f"  [green]ISO attached, boot order set[/green]")

    # Step 7: Start VM
    console.print("\n[bold]Step 7:[/bold] Starting VM")

    if dry_run:
        console.print(f"  [yellow]Would start VM {vmid}[/yellow]")
    else:
        assert client is not None
        client.start_vm(vmid)
        console.print(f"  [green]VM {vmid} started[/green]")

    # Step 8: Wait for VM to get its IP via cloud-init
    console.print(f"\n[bold]Step 8:[/bold] Waiting for VM to get IP address")

    if dry_run:
        console.print(
            f"  [yellow]Would wait for guest agent IP (timeout: {ssh_timeout}s)[/yellow]"
        )
    else:
        assert client is not None
        console.print(f"  Polling guest agent for IP...")
        installer_host = None
        start_time = time.time()
        while time.time() - start_time < ssh_timeout:
            installer_host = client.get_vm_ip(vmid)
            if installer_host:
                break
            time.sleep(5)
        if not installer_host:
            console.print(f"  [red]Timeout waiting for guest agent IP[/red]")
            console.print(
                f"  Check VM console: ssh root@{proxmox_host} qm terminal {vmid}"
            )
            raise RuntimeError("Guest agent IP timeout")
        console.print(f"  [green]Got IP: {installer_host}[/green]")

        console.print(f"  Waiting for SSH on {installer_host}...")
        if not wait_for_ssh(installer_host, timeout=ssh_timeout):
            console.print(f"  [red]Timeout waiting for SSH on {installer_host}[/red]")
            raise RuntimeError("SSH timeout")
        console.print(f"  [green]SSH accessible[/green]")

    console.print("\n[bold green]Done![/bold green]")
    if not dry_run:
        console.print(f"\nVM VMID: {vmid}")
        console.print(f"VM IP: {ip_alloc['address']}")
        console.print(
            f"\nNext: clan machines install {machine} root@{ip_alloc['address']}"
        )
