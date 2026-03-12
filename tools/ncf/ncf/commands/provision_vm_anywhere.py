"""Provision Proxmox VMs using nixos-anywhere.

This module implements the nixos-anywhere based VM provisioning workflow:
1. Create VM with metadata from NixOS config
2. Boot from installer ISO
3. Run nixos-anywhere to install the system
"""

import json
import shutil
import socket
import subprocess
import tempfile
import time
from pathlib import Path
from typing import Any, Optional

from rich.console import Console
from rich.panel import Panel

from .. import config
from ..external import ExternalToolError
from ..nix import NixRunner
from ..proxmox_api import ProxmoxClient
from ..secrets_inject import gather_secrets_for_machine, decrypt_secrets_to_tempdir
from . import iso_installer
from .provision_vm import (
    build_qm_create_command,
    query_nixos_config,
    validate_existing_vm,
)

console = Console()


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


def run_nixos_anywhere(
    machine: str,
    target_host: str,
    extra_files_dir: Optional[Path] = None,
    generate_hardware_config: bool = True,
    dry_run: bool = False,
) -> None:
    """Run nixos-anywhere to install NixOS.

    Args:
        machine: NixOS configuration name
        target_host: SSH target (e.g., "root@192.168.1.100")
        extra_files_dir: Directory with extra files to copy
        generate_hardware_config: Generate hardware-configuration.nix
        dry_run: Show what would be done
    """
    repo_root = config.find_repo_root()

    cmd = [
        "nix",
        "run",
        "github:nix-community/nixos-anywhere",
        "--",
        "--flake",
        f"{repo_root}#{machine}",
        "--target-host",
        target_host,
    ]

    if extra_files_dir:
        cmd.extend(["--extra-files", str(extra_files_dir)])

    if generate_hardware_config:
        hardware_config_path = f"machines/{machine}/hardware-configuration.nix"
        cmd.extend(
            [
                "--generate-hardware-config",
                "nixos-generate-config",
                hardware_config_path,
            ]
        )

    if dry_run:
        console.print(f"[yellow]Would run:[/yellow] {' '.join(cmd)}")
        return

    console.print(f"Running nixos-anywhere...")
    subprocess.run(cmd, cwd=repo_root, check=True)


def run(
    machine: str,
    proxmox_host: str,
    bridge: str = "vmbr0",
    ssh_timeout: int = 300,
    generate_hardware_config: bool = True,
    dry_run: bool = False,
) -> None:
    """Provision a Proxmox VM using nixos-anywhere.

    This workflow:
    1. Creates VM with configuration from NixOS
    2. Boots from installer ISO
    3. Waits for SSH access
    4. Runs nixos-anywhere to install the system
    5. Reboots into the installed system

    Args:
        machine: NixOS configuration name
        proxmox_host: Proxmox host to provision on
        bridge: Network bridge name
        ssh_timeout: Timeout for SSH connectivity in seconds
        generate_hardware_config: Generate hardware-configuration.nix
        dry_run: Show what would be done
    """
    console.print(
        Panel(
            f"Provisioning VM: [bold]{machine}[/bold] on {proxmox_host} via nixos-anywhere"
        )
    )

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
        cloud_init_snippet=None,  # Not using cloud-init with nixos-anywhere
    )

    if dry_run:
        console.print(f"  [yellow]Would run on {proxmox_host}:[/yellow]")
        console.print(f"    {' '.join(qm_cmd)}")
    else:
        console.print(f"  Creating VM {vmid}...")
        ssh_cmd = ["ssh", f"root@{proxmox_host}"] + qm_cmd
        subprocess.run(ssh_cmd, check=True)
        console.print(f"  [green]VM {vmid} created[/green]")

    # Step 5: Configure EFI disk if UEFI
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
            ssh_cmd = ["ssh", f"root@{proxmox_host}"] + efi_cmd
            subprocess.run(ssh_cmd, check=True)
            console.print(f"  [green]EFI disk configured[/green]")

    # Step 6: Configure disks from NixOS config
    console.print("\n[bold]Step 5b:[/bold] Configuring disks")
    disks = vm_config.get("disks", [])
    if not disks:
        # Default disk if none specified
        console.print("  No disks specified, creating default 32G disk")
        disk_cmd = ["qm", "set", str(vmid), "--scsi0", "local-zfs:32"]
        if dry_run:
            console.print(f"  [yellow]Would run: {' '.join(disk_cmd)}[/yellow]")
        else:
            ssh_cmd = ["ssh", f"root@{proxmox_host}"] + disk_cmd
            subprocess.run(ssh_cmd, check=True)
    else:
        for disk in disks:
            disk_type = disk.get("type")
            bus = disk.get("bus", "scsi")
            index = disk.get("index", 0)
            disk_key = f"{bus}{index}"

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

    # Step 7: Attach ISO and set boot order
    console.print("\n[bold]Step 6:[/bold] Attaching ISO and configuring boot")

    if dry_run:
        console.print(f"  [yellow]Would attach ISO: {iso_ref}[/yellow]")
        console.print("  [yellow]Would set boot order: ide2[/yellow]")
    else:
        assert client is not None
        client.attach_iso(vmid, iso_ref)
        client.set_boot_order(vmid, "order=ide2;scsi0")
        console.print(f"  [green]ISO attached, boot order set[/green]")

    # Step 8: Start VM
    console.print("\n[bold]Step 7:[/bold] Starting VM")

    if dry_run:
        console.print(f"  [yellow]Would start VM {vmid}[/yellow]")
    else:
        assert client is not None
        client.start_vm(vmid)
        console.print(f"  [green]VM {vmid} started[/green]")

    # Step 9: Wait for SSH
    console.print(f"\n[bold]Step 8:[/bold] Waiting for SSH ({ip_alloc['address']})")

    if dry_run:
        console.print(
            f"  [yellow]Would wait for SSH on {ip_alloc['address']} (timeout: {ssh_timeout}s)[/yellow]"
        )
    else:
        console.print(f"  Waiting up to {ssh_timeout}s for SSH...")
        if not wait_for_ssh(ip_alloc["address"], timeout=ssh_timeout):
            console.print(
                f"  [red]Timeout waiting for SSH on {ip_alloc['address']}[/red]"
            )
            console.print(
                "  Check VM console: ssh root@{proxmox_host} qm terminal {vmid}"
            )
            raise RuntimeError("SSH timeout")
        console.print(f"  [green]SSH accessible[/green]")

    # Step 10: Prepare extra files (secrets)
    console.print("\n[bold]Step 9:[/bold] Preparing secrets for injection")

    secrets = gather_secrets_for_machine(machine, runner)
    extra_files_dir: Optional[Path] = None

    if secrets:
        console.print(f"  Found {len(secrets)} secret(s) to inject")
        if not dry_run:
            extra_files_dir = decrypt_secrets_to_tempdir(secrets)
            console.print(f"  [green]Secrets prepared[/green]")
    else:
        console.print("  No secrets to inject")

    # Step 11: Run nixos-anywhere
    console.print("\n[bold]Step 10:[/bold] Running nixos-anywhere")

    target_host = f"root@{ip_alloc['address']}"

    try:
        run_nixos_anywhere(
            machine=machine,
            target_host=target_host,
            extra_files_dir=extra_files_dir,
            generate_hardware_config=generate_hardware_config,
            dry_run=dry_run,
        )
        if not dry_run:
            console.print(f"  [green]nixos-anywhere completed[/green]")
    finally:
        # Cleanup secrets directory
        if extra_files_dir and extra_files_dir.exists():
            shutil.rmtree(extra_files_dir, ignore_errors=True)

    # Step 12: Detach ISO and set boot order to disk
    console.print("\n[bold]Step 11:[/bold] Cleanup and reboot")

    if dry_run:
        console.print("  [yellow]Would detach ISO[/yellow]")
        console.print("  [yellow]Would set boot order to disk[/yellow]")
        console.print("  [yellow]Would reboot VM[/yellow]")
    else:
        assert client is not None
        client.detach_iso(vmid)
        client.set_boot_order(vmid, "order=scsi0")
        console.print(f"  [green]ISO detached, boot order set to disk[/green]")

        # nixos-anywhere should handle reboot, but let's be safe
        console.print("  Waiting for VM to be accessible...")
        time.sleep(30)  # Give time for reboot

        if wait_for_ssh(ip_alloc["address"], timeout=120):
            console.print(f"  [green]VM is up and running[/green]")
        else:
            console.print(f"  [yellow]VM may still be booting, check manually[/yellow]")

    console.print("\n[bold green]Done![/bold green]")
    if not dry_run:
        console.print(f"\nVM VMID: {vmid}")
        console.print(f"\nAccess via SSH: ssh root@{ip_alloc['address']}")
