"""Provision Proxmox LXC containers."""

import json
import subprocess
from pathlib import Path
from typing import Any

from rich.console import Console
from rich.panel import Panel

from .. import config
from ..nix import NixRunner
from ..proxmox_api import ProxmoxClient
from . import build

console = Console()


def normalize_size(size: str) -> str:
    """Normalize size string for Proxmox pct command.

    Proxmox pct expects sizes as plain numbers (in GB) for ZFS storage.
    This function strips the 'G' suffix if present.
    """
    if size.upper().endswith("G"):
        return size[:-1]
    return size


def query_nixos_config(runner: NixRunner, machine: str, attribute: str) -> Any:
    """Query an attribute from a NixOS configuration."""
    repo_root = config.find_repo_root()
    flake_ref = f"{repo_root}#nixosConfigurations.{machine}.{attribute}"
    result = runner.run_eval(flake_ref, json_output=True)
    return json.loads(result.stdout)


def copy_tarball_to_proxmox(
    local_tarball: Path, proxmox_host: str, machine: str
) -> str:
    """Copy tarball to Proxmox host. Returns the remote path."""
    remote_path = f"/var/lib/vz/template/cache/proxmox-lxc-{machine}.tar.xz"
    console.print(f"  Copying tarball to {proxmox_host}:{remote_path}")

    cmd = ["rsync", "-avP", str(local_tarball), f"root@{proxmox_host}:{remote_path}"]
    subprocess.run(cmd, check=True)

    return remote_path


def create_pct_command(
    vmid: int,
    tarball_path: str,
    hostname: str,
    lxc_config: dict[str, Any],
    network_config: dict[str, Any],
    bridge: str,
) -> list[str]:
    """Build the pct create command."""
    cmd = [
        "pct",
        "create",
        str(vmid),
        tarball_path,
        "--hostname",
        hostname,
        "--memory",
        str(lxc_config["memory"]),
        "--swap",
        str(lxc_config["swap"]),
        "--ostype",
        "nixos",
        "--unprivileged",
        "1" if lxc_config["unprivileged"] else "0",
        "--onboot",
        "1" if lxc_config["onboot"] else "0",
    ]

    # Network configuration
    ip_addr = network_config["address"]
    prefix = network_config["prefix"]
    gateway = network_config["gateway"]
    mac = network_config.get("mac")

    net_spec = f"name=eth0,bridge={bridge},firewall=1,gw={gateway},ip={ip_addr}/{prefix},type=veth"
    if mac:
        net_spec += f",hwaddr={mac}"
    cmd.extend(["--net0", net_spec])

    # Root filesystem
    rootfs = lxc_config["rootfs"]
    cmd.extend(["--rootfs", f"{rootfs['pool']}:{normalize_size(rootfs['size'])}"])

    # Additional mount points
    for i, mount in enumerate(lxc_config.get("mounts", [])):
        mount_spec = f"{mount['pool']}:{normalize_size(mount['size'])},mp={mount['mountPoint']}"
        if not mount.get("backup", True):
            mount_spec += ",backup=0"
        if not mount.get("replicate", True):
            mount_spec += ",replicate=0"
        cmd.extend([f"--mp{i}", mount_spec])

    # DNS
    dns_servers = network_config.get("dns", [])
    if dns_servers:
        cmd.extend(["--nameserver", " ".join(dns_servers)])

    # Search domain
    searchdomain = network_config.get("searchdomain")
    if searchdomain:
        cmd.extend(["--searchdomain", searchdomain])

    # Features (nesting for docker/podman)
    features = network_config.get("features", {})
    if features.get("nesting"):
        cmd.extend(["--features", "nesting=1"])

    return cmd


def restore_ssh_host_keys(
    proxmox_host: str, vmid: int, machine: str, repo_root: Path
) -> None:
    """Restore SSH host keys after Proxmox overwrites them.

    Proxmox generates fresh SSH host keys during container creation,
    overwriting our injected keys. This function restores the original
    keys from the secrets directory.
    """
    import tempfile

    secrets_dir = repo_root / "secrets" / machine

    # Get the container's rootfs path
    # For ZFS, it's typically /rpool/data/subvol-{vmid}-disk-0
    result = subprocess.run(
        ["ssh", f"root@{proxmox_host}", f"pct config {vmid} | grep rootfs"],
        capture_output=True,
        text=True,
        check=True,
    )
    # Parse rootfs line like: rootfs: local-zfs:subvol-100-disk-0,size=32G
    rootfs_line = result.stdout.strip()
    # Extract the volume name
    volume = rootfs_line.split(":")[2].split(",")[0]
    # For local-zfs, the path is /rpool/data/{volume}
    container_root = f"/rpool/data/{volume}"

    ssh_dir = f"{container_root}/etc/ssh"

    for key_type in ["ed25519", "rsa", "ecdsa"]:
        key_file = secrets_dir / f"ssh_host_{key_type}_key"
        pub_file = secrets_dir / f"ssh_host_{key_type}_key.pub"

        if key_file.exists():
            # Decrypt the key to a temp file
            with tempfile.NamedTemporaryFile(mode="w", delete=False) as tmp:
                tmp_path = tmp.name

            try:
                with open(tmp_path, "w") as out_file:
                    subprocess.run(
                        ["sops", "decrypt", str(key_file)],
                        stdout=out_file,
                        check=True,
                    )

                # Copy private key to container
                subprocess.run(
                    ["scp", tmp_path, f"root@{proxmox_host}:{ssh_dir}/ssh_host_{key_type}_key"],
                    check=True,
                )
                # Set correct permissions
                subprocess.run(
                    ["ssh", f"root@{proxmox_host}", f"chmod 600 {ssh_dir}/ssh_host_{key_type}_key"],
                    check=True,
                )
            finally:
                Path(tmp_path).unlink(missing_ok=True)

        if pub_file.exists():
            # Public keys are not encrypted
            subprocess.run(
                ["scp", str(pub_file), f"root@{proxmox_host}:{ssh_dir}/ssh_host_{key_type}_key.pub"],
                check=True,
            )
            subprocess.run(
                ["ssh", f"root@{proxmox_host}", f"chmod 644 {ssh_dir}/ssh_host_{key_type}_key.pub"],
                check=True,
            )


def append_extra_config(proxmox_host: str, vmid: int, extra_config: str) -> None:
    """Append extra configuration lines to the LXC config file."""
    if not extra_config.strip():
        return

    config_path = f"/etc/pve/lxc/{vmid}.conf"
    console.print(f"  Appending extra config to {config_path}")

    # Escape the config for shell
    escaped = extra_config.replace("'", "'\\''")
    cmd = [
        "ssh",
        f"root@{proxmox_host}",
        f"echo '{escaped}' >> {config_path}",
    ]
    subprocess.run(cmd, check=True)


def validate_existing_container(
    current_config: dict[str, Any],
    hostname: str,
    lxc_config: dict[str, Any],
) -> list[str]:
    """Validate existing container config against expected values.

    Args:
        current_config: Current container config from ProxmoxClient.get_container_config()
        hostname: Expected hostname
        lxc_config: Expected LXC config from NixOS

    Returns a list of mismatches.
    """
    mismatches = []

    # Check memory
    expected_memory = lxc_config["memory"]
    current_memory = current_config.get("memory")
    if current_memory != expected_memory:
        mismatches.append(f"memory: expected {expected_memory}, got {current_memory}")

    # Check swap
    expected_swap = lxc_config["swap"]
    current_swap = current_config.get("swap")
    if current_swap != expected_swap:
        mismatches.append(f"swap: expected {expected_swap}, got {current_swap}")

    # Check hostname
    current_hostname = current_config.get("hostname")
    if current_hostname != hostname:
        mismatches.append(f"hostname: expected {hostname}, got {current_hostname}")

    return mismatches


def run(
    machine: str,
    proxmox_host: str,
    bridge: str = "vmbr0",
    reuse_remote_tarball: bool = False,
    dry_run: bool = False,
) -> None:
    """Provision a Proxmox LXC container.

    Args:
        machine: NixOS configuration name
        proxmox_host: Proxmox host to provision on
        bridge: Network bridge name (default: vmbr0)
        reuse_remote_tarball: Skip tarball build/copy if remote exists
        dry_run: Show what would be done without executing
    """
    console.print(
        Panel(f"Provisioning LXC container: [bold]{machine}[/bold] on {proxmox_host}")
    )

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

    lxc_config = query_nixos_config(runner, machine, "config.proxmoxLXC")
    console.print(f"  Memory: {lxc_config['memory']} MB")
    console.print(f"  Swap: {lxc_config['swap']} MB")
    console.print(
        f"  Rootfs: {lxc_config['rootfs']['pool']}:{lxc_config['rootfs']['size']}"
    )
    if lxc_config.get("mounts"):
        console.print(f"  Mounts: {len(lxc_config['mounts'])} additional mount(s)")

    # Get network info
    ip_alloc = query_nixos_config(
        runner, machine, "config.inventory.hostIpAllocation.home.primary"
    )
    console.print(f"  IP: {ip_alloc['address']}")
    if ip_alloc.get("mac"):
        console.print(f"  MAC: {ip_alloc['mac']}")

    # Get network-wide info (dns, gateway, etc)
    network_info = query_nixos_config(runner, machine, "config.inventory.networks.home")

    # Check if docker/podman is enabled for nesting
    try:
        docker_enabled = query_nixos_config(
            runner, machine, "config.virtualisation.docker.enable"
        )
    except Exception:
        docker_enabled = False

    try:
        podman_enabled = query_nixos_config(
            runner, machine, "config.virtualisation.podman.enable"
        )
    except Exception:
        podman_enabled = False

    needs_nesting = docker_enabled or podman_enabled
    if needs_nesting:
        console.print("  [dim]Nesting enabled (docker/podman detected)[/dim]")

    network_config = {
        "address": ip_alloc["address"],
        "prefix": network_info["prefix"],
        "gateway": network_info["gateway"],
        "dns": network_info.get("dns", []),
        "searchdomain": network_info.get("searchdomain"),
        "mac": ip_alloc.get("mac"),
        "features": {"nesting": needs_nesting},
    }

    # Step 2: Check if container exists
    console.print("\n[bold]Step 2:[/bold] Checking for existing container")

    if dry_run:
        console.print(f"  [yellow]Would check for container '{hostname}'[/yellow]")
        existing_vmid = None
    else:
        assert client is not None
        existing_vmid = client.container_exists(hostname)

    if existing_vmid:
        console.print(
            f"  [yellow]Container '{hostname}' exists with VMID {existing_vmid}[/yellow]"
        )
        console.print("\n[bold]Validation mode:[/bold] Comparing configuration")

        if not dry_run:
            assert client is not None
            current_config = client.get_container_config(existing_vmid)
            mismatches = validate_existing_container(
                current_config, hostname, lxc_config
            )

            if mismatches:
                console.print("\n[red]Configuration mismatches found:[/red]")
                for m in mismatches:
                    console.print(f"  - {m}")
            else:
                console.print("[green]Configuration matches expected values[/green]")

        console.print(
            "\n[yellow]Existing container not modified. "
            "Delete it manually if you want to reprovision.[/yellow]"
        )
        return

    console.print(f"  [green]Container '{hostname}' does not exist[/green]")

    # Step 3: Build tarball
    local_tarball = repo_root / f"proxmox-lxc-{machine}.tar.xz"
    remote_tarball = f"/var/lib/vz/template/cache/proxmox-lxc-{machine}.tar.xz"

    should_build = True
    if reuse_remote_tarball:
        console.print("\n[bold]Step 3:[/bold] Checking for existing remote tarball")
        if dry_run:
            console.print(
                f"  [yellow]Would check for {remote_tarball} on {proxmox_host}[/yellow]"
            )
        else:
            check_cmd = [
                "ssh",
                f"root@{proxmox_host}",
                f"test -f {remote_tarball} && echo exists",
            ]
            result = subprocess.run(check_cmd, capture_output=True, text=True)
            if "exists" in result.stdout:
                console.print(f"  [green]Remote tarball exists, skipping build[/green]")
                should_build = False

    if should_build:
        console.print("\n[bold]Step 3:[/bold] Building LXC tarball with secrets")
        if dry_run:
            console.print(
                f"  [yellow]Would build: ncf build lxc {machine} --inject-secrets[/yellow]"
            )
        else:
            build.run_lxc(
                target=machine,
                output=local_tarball,
                verbosity=1,
                inject_secrets=True,
            )

    # Step 4: Copy tarball to Proxmox
    if should_build:
        console.print("\n[bold]Step 4:[/bold] Copying tarball to Proxmox")
        if dry_run:
            console.print(
                f"  [yellow]Would copy {local_tarball} to {proxmox_host}:{remote_tarball}[/yellow]"
            )
        else:
            copy_tarball_to_proxmox(local_tarball, proxmox_host, machine)

    # Step 5: Get next VMID and create container
    console.print("\n[bold]Step 5:[/bold] Creating container")

    if dry_run:
        vmid = 999
        console.print("  [yellow]Would get next VMID from Proxmox[/yellow]")
    else:
        assert client is not None
        vmid = client.get_next_vmid()
        console.print(f"  Next available VMID: {vmid}")

    pct_cmd = create_pct_command(
        vmid=vmid,
        tarball_path=remote_tarball,
        hostname=hostname,
        lxc_config=lxc_config,
        network_config=network_config,
        bridge=bridge,
    )

    if dry_run:
        console.print(f"  [yellow]Would run on {proxmox_host}:[/yellow]")
        console.print(f"    {' '.join(pct_cmd)}")
    else:
        console.print(f"  Creating container {vmid}...")
        ssh_cmd = ["ssh", f"root@{proxmox_host}"] + pct_cmd
        subprocess.run(ssh_cmd, check=True)
        console.print(f"  [green]Container {vmid} created[/green]")

    # Step 5b: Restore SSH host keys (Proxmox overwrites them during creation)
    if not dry_run:
        console.print("\n[bold]Step 5b:[/bold] Restoring SSH host keys")
        restore_ssh_host_keys(proxmox_host, vmid, machine, repo_root)
        console.print("  [green]SSH host keys restored[/green]")

    # Step 6: Append extra config
    extra_config = lxc_config.get("extraConfig", "")
    if extra_config.strip():
        console.print("\n[bold]Step 6:[/bold] Appending extra configuration")
        if dry_run:
            console.print("  [yellow]Would append extra config:[/yellow]")
            for line in extra_config.strip().split("\n"):
                console.print(f"    {line}")
        else:
            append_extra_config(proxmox_host, vmid, extra_config)
            console.print("  [green]Extra config appended[/green]")

    # Step 7: Cleanup
    if should_build and not reuse_remote_tarball:
        console.print("\n[bold]Step 7:[/bold] Cleanup")
        if dry_run:
            console.print(f"  [yellow]Would remove {local_tarball}[/yellow]")
            console.print(
                f"  [yellow]Would remove {remote_tarball} on {proxmox_host}[/yellow]"
            )
        else:
            # Remove local tarball
            if local_tarball.exists():
                local_tarball.unlink()
                console.print(f"  Removed local tarball: {local_tarball}")

            # Remove remote tarball
            rm_cmd = ["ssh", f"root@{proxmox_host}", f"rm -f {remote_tarball}"]
            subprocess.run(rm_cmd, check=True)
            console.print(f"  Removed remote tarball: {remote_tarball}")

    console.print("\n[bold green]Done![/bold green]")
    if not dry_run:
        console.print(f"\nContainer VMID: {vmid}")
        console.print("\nNext steps:")
        console.print(
            f"  1. Start the container: ssh root@{proxmox_host} pct start {vmid}"
        )
        console.print(
            f"  2. Enter the container: ssh root@{proxmox_host} pct enter {vmid}"
        )
        console.print(f"  3. Or SSH directly: ssh root@{ip_alloc['address']}")
