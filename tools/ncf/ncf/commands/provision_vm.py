"""Provision Proxmox VMs."""

import base64
import json
import subprocess
from pathlib import Path
from typing import Any

import yaml
from rich.console import Console
from rich.panel import Panel

from .. import config
from ..nix import NixRunner
from ..proxmox_api import ProxmoxClient
from ..secrets_inject import gather_secrets_for_machine, decrypt_secrets_to_tempdir
from . import build

console = Console()


def query_nixos_config(runner: NixRunner, machine: str, attribute: str) -> Any:
    """Query an attribute from a NixOS configuration."""
    repo_root = config.find_repo_root()
    flake_ref = f"{repo_root}#nixosConfigurations.{machine}.{attribute}"
    result = runner.run_eval(flake_ref, json_output=True)
    return json.loads(result.stdout)


def generate_cloud_init_userdata(
    hostname: str,
    ssh_keys: list[dict[str, Any]],
    authorized_keys: list[str],
) -> str:
    """Generate cloud-init user-data YAML.

    Args:
        hostname: The VM hostname
        ssh_keys: List of SSH host key dicts with 'path' and 'content' (base64)
        authorized_keys: List of SSH authorized keys for root

    Returns:
        YAML string for cloud-init user-data
    """
    userdata: dict[str, Any] = {
        "hostname": hostname,
        "manage_etc_hosts": False,  # NixOS manages /etc/hosts
        "ssh_authorized_keys": authorized_keys,
    }

    # Write SSH host keys
    write_files = []
    for key in ssh_keys:
        path = key["path"]
        content = key["content"]
        is_public = path.endswith(".pub")

        write_files.append(
            {
                "path": path,
                "permissions": "0644" if is_public else "0600",
                "encoding": "base64",
                "content": content,
            }
        )

    if write_files:
        userdata["write_files"] = write_files

    # Add cloud-config header
    yaml_content = yaml.dump(userdata, default_flow_style=False, sort_keys=False)
    return f"#cloud-config\n{yaml_content}"


def build_qm_create_command(
    vmid: int,
    hostname: str,
    vm_config: dict[str, Any],
    network_config: dict[str, Any],
    cloud_init_snippet: str | None = None,
) -> list[str]:
    """Build the qm create command.

    Args:
        vmid: The VM ID
        hostname: VM hostname
        vm_config: proxmox VM configuration from NixOS config
        network_config: Network configuration with IP, gateway, etc.
        cloud_init_snippet: Path to cloud-init snippet (e.g., "local:snippets/foo.yaml")

    Returns:
        Command list for qm create
    """
    cmd = [
        "qm",
        "create",
        str(vmid),
        "--name",
        hostname,
        "--memory",
        str(vm_config["memory"]),
        "--cores",
        str(vm_config["cores"]),
        "--sockets",
        str(vm_config.get("sockets", 1)),
        "--bios",
        vm_config.get("bios", "seabios"),
        "--machine",
        vm_config.get("machine", "q35"),
        "--scsihw",
        vm_config.get("scsihw", "virtio-scsi-single"),
    ]

    if vm_config.get("onboot", False):
        cmd.extend(["--onboot", "1"])

    if vm_config.get("agent", True):
        cmd.extend(["--agent", "1"])

    # Balloon memory configuration
    balloon = vm_config.get("balloon")
    if balloon is not None:
        cmd.extend(["--balloon", str(balloon)])

    shares = vm_config.get("shares", 1000)
    if shares != 1000:  # Only include if non-default
        cmd.extend(["--shares", str(shares)])

    description = vm_config.get("description")
    if description:
        cmd.extend(["--description", description])

    # Network configuration
    network = vm_config.get("network", {})
    bridge = network.get("bridge", "vmbr0")
    model = network.get("model", "virtio")
    firewall = "1" if network.get("firewall", True) else "0"

    # Include MAC address if available
    mac = network_config.get("mac")
    net_spec = f"{model},bridge={bridge},firewall={firewall}"
    if mac:
        net_spec += f",macaddr={mac}"
    cmd.extend(["--net0", net_spec])

    # Serial console for NixOS (common for cloud images)
    cmd.extend(["--serial0", "socket", "--vga", "serial0"])

    # Cloud-init snippet
    if cloud_init_snippet:
        cmd.extend(["--cicustom", f"user={cloud_init_snippet}"])

        # Also set ipconfig0 for static IP
        ip_addr = network_config.get("address")
        prefix = network_config.get("prefix")
        gateway = network_config.get("gateway")
        if ip_addr and prefix and gateway:
            cmd.extend(["--ipconfig0", f"ip={ip_addr}/{prefix},gw={gateway}"])

    return cmd


def run(
    machine: str,
    proxmox_host: str,
    bridge: str = "vmbr0",
    start: bool = False,
    dry_run: bool = False,
) -> None:
    """Provision a Proxmox VM.

    Args:
        machine: NixOS configuration name
        proxmox_host: Proxmox host to provision on
        bridge: Network bridge name (default: vmbr0)
        start: Start the VM after provisioning
        dry_run: Show what would be done without executing
    """
    console.print(Panel(f"Provisioning VM: [bold]{machine}[/bold] on {proxmox_host}"))

    repo_root = config.find_repo_root()
    runner = NixRunner(verbosity=1, repo_root=repo_root)

    # Initialize Proxmox client (unless dry run)
    client: ProxmoxClient | None = None
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
    balloon = vm_config.get("balloon")
    if balloon is not None:
        console.print(f"  Balloon (min memory): {balloon} MB")
        shares = vm_config.get("shares", 1000)
        console.print(f"  Shares: {shares}")
    console.print(f"  Cores: {vm_config['cores']}")
    console.print(f"  BIOS: {vm_config.get('bios', 'seabios')}")

    tpm2_config = vm_config.get("tpm2", {})
    if tpm2_config.get("enable", False):
        console.print(f"  TPM2: enabled (version {tpm2_config.get('version', 'v2.0')})")

    cloud_init_config = vm_config.get("cloudInit", {})
    cloud_init_enabled = cloud_init_config.get("enable", True)
    console.print(f"  Cloud-init: {'enabled' if cloud_init_enabled else 'disabled'}")

    # Get network info
    ip_alloc = query_nixos_config(
        runner, machine, "config.inventory.hostIpAllocation.home.primary"
    )
    console.print(f"  IP: {ip_alloc['address']}")
    if ip_alloc.get("mac"):
        console.print(f"  MAC: {ip_alloc['mac']}")

    # Get network-wide info (dns, gateway, etc)
    network_info = query_nixos_config(runner, machine, "config.inventory.networks.home")

    network_config = {
        "address": ip_alloc["address"],
        "prefix": network_info["prefix"],
        "gateway": network_info["gateway"],
        "dns": network_info.get("dns", []),
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

    # Step 3: Build disko images
    console.print("\n[bold]Step 3:[/bold] Building disko images")

    if dry_run:
        console.print(f"  [yellow]Would build: ncf build vm {machine}[/yellow]")
        image_path = Path("/path/to/image.raw")
    else:
        # Build the disko images
        build.run_vm(
            target=machine,
            output=None,  # Use default output path
            verbosity=1,
        )
        # Find the built image
        image_path = find_disko_image(repo_root, machine)
        console.print(f"  Built image: {image_path}")

    # Step 4: Generate cloud-init user-data (if enabled)
    cloud_init_snippet_ref = None
    if cloud_init_enabled:
        console.print("\n[bold]Step 4:[/bold] Generating cloud-init configuration")

        # Gather secrets for cloud-init
        secrets = gather_secrets_for_machine(machine, runner)
        ssh_keys = []

        if secrets and not dry_run:
            import shutil
            import tempfile

            secrets_dir = decrypt_secrets_to_tempdir(secrets)
            try:
                for secret in secrets:
                    if (
                        "/ssh/" in secret.target_path
                        or "ssh_host" in secret.target_path
                    ):
                        file_path = secrets_dir / secret.target_path.lstrip("/")
                        if file_path.exists():
                            content = file_path.read_bytes()
                            ssh_keys.append(
                                {
                                    "path": secret.target_path,
                                    "content": base64.b64encode(content).decode(),
                                }
                            )
            finally:
                shutil.rmtree(secrets_dir, ignore_errors=True)

        # Get authorized keys from inventory
        authorized_keys = query_nixos_config(
            runner,
            machine,
            "config.users.users.root.openssh.authorizedKeys.keys",
        )

        userdata = generate_cloud_init_userdata(
            hostname=hostname,
            ssh_keys=ssh_keys,
            authorized_keys=authorized_keys,
        )

        snippet_filename = f"{hostname}-cloud-init.yaml"
        cloud_init_storage = cloud_init_config.get("storage", "local")
        cloud_init_snippet_ref = f"{cloud_init_storage}:snippets/{snippet_filename}"

        if dry_run:
            console.print(
                f"  [yellow]Would upload snippet: {snippet_filename}[/yellow]"
            )
            console.print("  [dim]Cloud-init user-data preview:[/dim]")
            for line in userdata.split("\n")[:10]:
                console.print(f"    {line}")
            if userdata.count("\n") > 10:
                console.print("    ...")
        else:
            assert client is not None
            console.print(f"  Uploading snippet: {snippet_filename}")
            client.upload_snippet(cloud_init_storage, snippet_filename, userdata)
            console.print(f"  [green]Snippet uploaded[/green]")

    # Step 5: Get next VMID and create VM
    console.print("\n[bold]Step 5:[/bold] Creating VM")

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
        cloud_init_snippet=cloud_init_snippet_ref,
    )

    if dry_run:
        console.print(f"  [yellow]Would run on {proxmox_host}:[/yellow]")
        console.print(f"    {' '.join(qm_cmd)}")
    else:
        console.print(f"  Creating VM {vmid}...")
        ssh_cmd = ["ssh", f"root@{proxmox_host}"] + qm_cmd
        subprocess.run(ssh_cmd, check=True)
        console.print(f"  [green]VM {vmid} created[/green]")

    # Step 6: Configure EFI disk (if UEFI)
    if vm_config.get("bios") == "ovmf":
        console.print("\n[bold]Step 6:[/bold] Configuring EFI disk")
        efidisk = vm_config.get("efidisk", {})
        storage = efidisk.get("storage", "local-zfs")
        efitype = efidisk.get("efitype", "4m")
        secure_boot = efidisk.get("secureBoot", False)

        # pre-enrolled-keys=1 enables Secure Boot by pre-loading MS/distro keys
        # pre-enrolled-keys=0 disables Secure Boot (default for easier custom setups)
        pre_enrolled = "1" if secure_boot else "0"
        efi_spec = f"{storage}:1,efitype={efitype},pre-enrolled-keys={pre_enrolled}"

        efi_cmd = [
            "qm",
            "set",
            str(vmid),
            "--efidisk0",
            efi_spec,
        ]

        if dry_run:
            console.print(
                f"  [yellow]Secure Boot: {'enabled' if secure_boot else 'disabled'}[/yellow]"
            )
            console.print(f"  [yellow]Would run: {' '.join(efi_cmd)}[/yellow]")
        else:
            console.print(f"  Secure Boot: {'enabled' if secure_boot else 'disabled'}")
            ssh_cmd = ["ssh", f"root@{proxmox_host}"] + efi_cmd
            subprocess.run(ssh_cmd, check=True)
            console.print(f"  [green]EFI disk configured[/green]")

    # Step 7: Configure TPM2 (if enabled)
    if tpm2_config.get("enable", False):
        console.print("\n[bold]Step 7:[/bold] Configuring TPM2")
        tpm_storage = tpm2_config.get("storage", "local-zfs")
        tpm_version = tpm2_config.get("version", "v2.0")

        tpm_cmd = [
            "qm",
            "set",
            str(vmid),
            "--tpmstate0",
            f"{tpm_storage}:1,version={tpm_version}",
        ]

        if dry_run:
            console.print(f"  [yellow]Would run: {' '.join(tpm_cmd)}[/yellow]")
        else:
            ssh_cmd = ["ssh", f"root@{proxmox_host}"] + tpm_cmd
            subprocess.run(ssh_cmd, check=True)
            console.print(f"  [green]TPM2 configured[/green]")

    # Step 8: Import disk image
    console.print("\n[bold]Step 8:[/bold] Importing disk image")

    # Determine storage for disk import
    # Use the first disk's storage if defined, otherwise local-zfs
    disks = vm_config.get("disks", [])
    disk_storage = "local-zfs"
    if disks and disks[0].get("storage"):
        disk_storage = disks[0]["storage"]

    if dry_run:
        console.print(
            f"  [yellow]Would copy image to {proxmox_host} and import[/yellow]"
        )
    else:
        # Copy image to Proxmox host
        remote_image = f"/tmp/{machine}-disk.raw"
        console.print(f"  Copying image to {proxmox_host}...")
        subprocess.run(
            ["rsync", "-avP", str(image_path), f"root@{proxmox_host}:{remote_image}"],
            check=True,
        )

        # Import the disk
        console.print(f"  Importing disk to {disk_storage}...")
        import_cmd = ["qm", "importdisk", str(vmid), remote_image, disk_storage]
        ssh_cmd = ["ssh", f"root@{proxmox_host}"] + import_cmd
        result = subprocess.run(ssh_cmd, capture_output=True, text=True, check=True)

        # Parse the output to get the disk name
        # Output is like: "Successfully imported disk as 'unused0:local-zfs:vm-123-disk-0'"
        disk_name = None
        for line in result.stdout.split("\n"):
            if "Successfully imported" in line and ":" in line:
                # Extract the disk reference
                parts = line.split("'")
                if len(parts) >= 2:
                    disk_ref = parts[1].split(":")[1] + ":" + parts[1].split(":")[2]
                    disk_name = disk_ref

        if disk_name:
            console.print(f"  Disk imported as: {disk_name}")

            # Attach the disk
            attach_cmd = ["qm", "set", str(vmid), "--scsi0", disk_name]
            ssh_cmd = ["ssh", f"root@{proxmox_host}"] + attach_cmd
            subprocess.run(ssh_cmd, check=True)
            console.print(f"  [green]Disk attached[/green]")

            # Set boot order
            boot_cmd = ["qm", "set", str(vmid), "--boot", "order=scsi0"]
            ssh_cmd = ["ssh", f"root@{proxmox_host}"] + boot_cmd
            subprocess.run(ssh_cmd, check=True)
            console.print(f"  [green]Boot order set[/green]")
        else:
            console.print(
                f"  [yellow]Could not parse disk name from import output[/yellow]"
            )
            console.print(f"  Output: {result.stdout}")

        # Cleanup remote image
        subprocess.run(
            ["ssh", f"root@{proxmox_host}", f"rm -f {remote_image}"], check=True
        )
        console.print(f"  Removed temporary image")

    # Step 9: Start VM (if requested)
    if start:
        console.print("\n[bold]Step 9:[/bold] Starting VM")
        if dry_run:
            console.print(f"  [yellow]Would start VM {vmid}[/yellow]")
        else:
            assert client is not None
            client.start_vm(vmid)
            console.print(f"  [green]VM {vmid} started[/green]")

    console.print("\n[bold green]Done![/bold green]")
    if not dry_run:
        console.print(f"\nVM VMID: {vmid}")
        console.print("\nNext steps:")
        if not start:
            console.print(f"  1. Start the VM: ssh root@{proxmox_host} qm start {vmid}")
        console.print(
            f"  2. Access console: ssh root@{proxmox_host} qm terminal {vmid}"
        )
        console.print(f"  3. Or SSH directly: ssh root@{ip_alloc['address']}")


def validate_existing_vm(
    current_config: dict[str, Any],
    hostname: str,
    vm_config: dict[str, Any],
) -> list[str]:
    """Validate existing VM config against expected values.

    Returns a list of mismatches.
    """
    mismatches = []

    # Check memory
    expected_memory = vm_config["memory"]
    current_memory = current_config.get("memory")
    if current_memory != expected_memory:
        mismatches.append(f"memory: expected {expected_memory}, got {current_memory}")

    # Check cores
    expected_cores = vm_config["cores"]
    current_cores = current_config.get("cores")
    if current_cores != expected_cores:
        mismatches.append(f"cores: expected {expected_cores}, got {current_cores}")

    # Check hostname
    current_name = current_config.get("name")
    if current_name != hostname:
        mismatches.append(f"name: expected {hostname}, got {current_name}")

    # Check balloon memory
    expected_balloon = vm_config.get("balloon")
    current_balloon = current_config.get("balloon")
    if expected_balloon is not None and current_balloon != expected_balloon:
        mismatches.append(
            f"balloon: expected {expected_balloon}, got {current_balloon}"
        )

    # Check shares (only if balloon is configured)
    if expected_balloon is not None:
        expected_shares = vm_config.get("shares", 1000)
        current_shares = current_config.get("shares", 1000)
        if current_shares != expected_shares:
            mismatches.append(
                f"shares: expected {expected_shares}, got {current_shares}"
            )

    return mismatches


def find_disko_image(repo_root: Path, machine: str) -> Path:
    """Find the disko image for a machine.

    The disko image is typically at result/main.raw or similar.
    """
    # Look for common disko image locations
    possible_paths = [
        repo_root / "result" / "main.raw",
        repo_root / "result" / f"{machine}.raw",
        repo_root / f"{machine}-disko-images" / "main.raw",
    ]

    for path in possible_paths:
        if path.exists():
            return path

    # Try to find any .raw file in result directory
    result_dir = repo_root / "result"
    if result_dir.exists():
        raw_files = list(result_dir.glob("**/*.raw"))
        if raw_files:
            return raw_files[0]

    raise FileNotFoundError(
        f"Could not find disko image for {machine}. "
        f"Checked: {', '.join(str(p) for p in possible_paths)}"
    )
