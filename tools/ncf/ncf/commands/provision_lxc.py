"""Provision Proxmox LXC containers."""

import json
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Any, Optional

from rich.console import Console
from rich.panel import Panel

from .. import config
from ..external import ExternalToolError
from ..nix import NixRunner
from ..proxmox_api import ProxmoxClient
from ..secrets_inject import (
    gather_secrets_for_machine,
    decrypt_secrets_to_tempdir,
)
from . import build
from .build import (
    _find_tarball_in_result,
    _detect_compression,
    _get_decompress_command,
    _get_compress_command,
    _group_secrets_by_owner,
)

console = Console()


def _format_size(size_bytes: int) -> str:
    """Format a size in bytes as a human-readable string."""
    for unit in ["B", "KB", "MB", "GB", "TB"]:
        if size_bytes < 1024:
            return f"{size_bytes:.1f} {unit}"
        size_bytes /= 1024
    return f"{size_bytes:.1f} PB"


def normalize_size(size: str) -> str:
    """Normalize size string for Proxmox pct command.

    Proxmox pct expects sizes as plain numbers (in GB) for ZFS storage.
    This function converts sizes to GB and strips any suffix.
    """
    size_upper = size.upper()
    if size_upper.endswith("T"):
        # Convert terabytes to gigabytes
        return str(int(size[:-1]) * 1024)
    if size_upper.endswith("G"):
        return size[:-1]
    return size


def query_nixos_config(runner: NixRunner, machine: str, attribute: str) -> Any:
    """Query an attribute from a NixOS configuration."""
    repo_root = config.find_repo_root()
    flake_ref = f"{repo_root}#nixosConfigurations.{machine}.{attribute}"
    result = runner.run_eval(flake_ref, json_output=True)
    return json.loads(result.stdout)


def copy_tarball_to_proxmox(
    local_tarball: Path, proxmox_host: str, machine: str, compression: str = "zstd"
) -> str:
    """Copy tarball to Proxmox host. Returns the remote path."""
    ext = ".tar.zst" if compression == "zstd" else ".tar.xz"
    remote_path = f"/var/lib/vz/template/cache/proxmox-lxc-{machine}{ext}"
    console.print(f"  Copying tarball to {proxmox_host}:{remote_path}")

    cmd = ["rsync", "-avP", str(local_tarball), f"root@{proxmox_host}:{remote_path}"]
    subprocess.run(cmd, check=True)

    return remote_path


def stream_tarball_to_remote(
    source_tarball: Path,
    machine_name: str,
    proxmox_host: str,
    runner: NixRunner,
    compression: str = "zstd",
) -> str:
    """Stream tarball with injected secrets directly to remote.

    This avoids creating a local copy of the secrets-injected tarball.
    The pipeline is:
    decompress source | bsdtar (add secrets) | compress | ssh cat > remote

    Args:
        source_tarball: Path to the source tarball from nix build
        machine_name: NixOS configuration name for gathering secrets
        proxmox_host: Remote Proxmox host
        runner: NixRunner instance for nix eval
        compression: Output compression format

    Returns:
        Remote path where tarball was written
    """
    ext = ".tar.zst" if compression == "zstd" else ".tar.xz"
    remote_path = f"/var/lib/vz/template/cache/proxmox-lxc-{machine_name}{ext}"

    console.print(f"  Streaming tarball to {proxmox_host}:{remote_path}")

    # Gather secrets
    secrets = gather_secrets_for_machine(machine_name, runner)
    if not secrets:
        console.print("  [yellow]No secrets to inject, copying directly[/yellow]")
        # Stream source directly
        source_compression = _detect_compression(source_tarball)
        if source_compression == compression:
            # Same format, just copy
            cmd = [
                "rsync",
                "-avP",
                str(source_tarball),
                f"root@{proxmox_host}:{remote_path}",
            ]
            subprocess.run(cmd, check=True)
        else:
            # Need to recompress - use pv for progress
            source_size = source_tarball.stat().st_size
            console.print(f"  Source size: {_format_size(source_size)}")

            decompress_cmd = _get_decompress_command(source_compression)
            compress_cmd = _get_compress_command(compression)
            pv_cmd = ["pv", "-s", str(source_size), "-N", "Streaming"]
            ssh_cmd = ["ssh", f"root@{proxmox_host}", f"cat > {remote_path}"]

            # Pipeline: decompress | compress | pv | ssh
            decompress_proc = subprocess.Popen(
                decompress_cmd + [str(source_tarball)], stdout=subprocess.PIPE
            )
            compress_proc = subprocess.Popen(
                compress_cmd, stdin=decompress_proc.stdout, stdout=subprocess.PIPE
            )
            if decompress_proc.stdout:
                decompress_proc.stdout.close()
            pv_proc = subprocess.Popen(
                pv_cmd, stdin=compress_proc.stdout, stdout=subprocess.PIPE
            )
            if compress_proc.stdout:
                compress_proc.stdout.close()
            ssh_proc = subprocess.Popen(ssh_cmd, stdin=pv_proc.stdout)
            if pv_proc.stdout:
                pv_proc.stdout.close()

            # Wait for all processes
            ssh_rc = ssh_proc.wait()
            pv_rc = pv_proc.wait()
            compress_rc = compress_proc.wait()
            decompress_rc = decompress_proc.wait()

            if decompress_rc != 0:
                raise ExternalToolError(
                    decompress_cmd[0],
                    f"Decompression failed with exit code {decompress_rc}",
                    decompress_rc,
                )
            if compress_rc != 0:
                raise ExternalToolError(
                    compress_cmd[0],
                    f"Compression failed with exit code {compress_rc}",
                    compress_rc,
                )
            if pv_rc != 0:
                raise ExternalToolError(
                    "pv", f"pv failed with exit code {pv_rc}", pv_rc
                )
            if ssh_rc != 0:
                raise ExternalToolError(
                    "ssh", f"SSH copy failed with exit code {ssh_rc}", ssh_rc
                )
            console.print("  [green]Streaming complete[/green]")
        return remote_path

    console.print(f"  Found {len(secrets)} secret(s) to inject")

    # Group secrets by owner/group
    secret_groups = _group_secrets_by_owner(secrets)
    console.print(f"  Ownership groups: {len(secret_groups)}")

    # Track temp directories and files for cleanup
    temp_dirs: list[Path] = []
    temp_tar_files: list[Path] = []

    try:
        # Create small tar archives for each ownership group
        console.print("  Preparing secrets archives...")
        for (owner, group), group_secrets in secret_groups.items():
            # Decrypt secrets to temp directory
            secrets_dir = decrypt_secrets_to_tempdir(group_secrets)
            temp_dirs.append(secrets_dir)

            # Create a small tar archive with correct ownership using GNU tar
            with tempfile.NamedTemporaryFile(
                prefix=f"ncf-secrets-{owner}-", suffix=".tar", delete=False
            ) as temp_tar:
                temp_tar_path = Path(temp_tar.name)
                temp_tar_files.append(temp_tar_path)

            subprocess.run(
                [
                    "tar",
                    "cf",
                    str(temp_tar_path),
                    f"--owner={owner}",
                    f"--group={group}",
                    "-C",
                    str(secrets_dir),
                    ".",
                ],
                check=True,
            )

        # Build the streaming pipeline with progress monitoring
        source_size = source_tarball.stat().st_size
        console.print(f"  Source size: {_format_size(source_size)}")

        source_compression = _detect_compression(source_tarball)
        decompress_cmd = _get_decompress_command(source_compression)
        compress_cmd = _get_compress_command(compression)
        pv_cmd = ["pv", "-s", str(source_size), "-N", "Streaming"]
        ssh_cmd = ["ssh", f"root@{proxmox_host}", f"cat > {remote_path}"]

        # Build bsdtar command
        bsdtar_cmd = ["bsdtar", "cf", "-", "@-"]
        for tar_file in temp_tar_files:
            bsdtar_cmd.append(f"@{tar_file}")

        # Run the pipeline: decompress | bsdtar | compress | pv | ssh
        decompress_proc = subprocess.Popen(
            decompress_cmd + [str(source_tarball)],
            stdout=subprocess.PIPE,
        )

        bsdtar_proc = subprocess.Popen(
            bsdtar_cmd,
            stdin=decompress_proc.stdout,
            stdout=subprocess.PIPE,
        )
        if decompress_proc.stdout:
            decompress_proc.stdout.close()

        compress_proc = subprocess.Popen(
            compress_cmd,
            stdin=bsdtar_proc.stdout,
            stdout=subprocess.PIPE,
        )
        if bsdtar_proc.stdout:
            bsdtar_proc.stdout.close()

        pv_proc = subprocess.Popen(
            pv_cmd,
            stdin=compress_proc.stdout,
            stdout=subprocess.PIPE,
        )
        if compress_proc.stdout:
            compress_proc.stdout.close()

        ssh_proc = subprocess.Popen(
            ssh_cmd,
            stdin=pv_proc.stdout,
        )
        if pv_proc.stdout:
            pv_proc.stdout.close()

        # Wait for all processes
        ssh_rc = ssh_proc.wait()
        pv_rc = pv_proc.wait()
        compress_rc = compress_proc.wait()
        bsdtar_rc = bsdtar_proc.wait()
        decompress_rc = decompress_proc.wait()

        if decompress_rc != 0:
            raise ExternalToolError(
                decompress_cmd[0],
                f"Decompression failed with exit code {decompress_rc}",
                decompress_rc,
            )
        if bsdtar_rc != 0:
            raise ExternalToolError(
                "bsdtar", f"bsdtar failed with exit code {bsdtar_rc}", bsdtar_rc
            )
        if compress_rc != 0:
            raise ExternalToolError(
                compress_cmd[0],
                f"Compression failed with exit code {compress_rc}",
                compress_rc,
            )
        if pv_rc != 0:
            raise ExternalToolError("pv", f"pv failed with exit code {pv_rc}", pv_rc)
        if ssh_rc != 0:
            raise ExternalToolError(
                "ssh", f"SSH copy failed with exit code {ssh_rc}", ssh_rc
            )

        console.print("  [green]Streaming complete[/green]")
        return remote_path

    finally:
        # Clean up temp tar files
        for tar_file in temp_tar_files:
            if tar_file.exists():
                tar_file.unlink()
        # Clean up all secrets directories
        for temp_dir in temp_dirs:
            shutil.rmtree(temp_dir, ignore_errors=True)


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
        mount_spec = (
            f"{mount['pool']}:{normalize_size(mount['size'])},mp={mount['mountPoint']}"
        )
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
                    [
                        "scp",
                        tmp_path,
                        f"root@{proxmox_host}:{ssh_dir}/ssh_host_{key_type}_key",
                    ],
                    check=True,
                )
                # Set correct permissions
                subprocess.run(
                    [
                        "ssh",
                        f"root@{proxmox_host}",
                        f"chmod 600 {ssh_dir}/ssh_host_{key_type}_key",
                    ],
                    check=True,
                )
            finally:
                Path(tmp_path).unlink(missing_ok=True)

        if pub_file.exists():
            # Public keys are not encrypted
            subprocess.run(
                [
                    "scp",
                    str(pub_file),
                    f"root@{proxmox_host}:{ssh_dir}/ssh_host_{key_type}_key.pub",
                ],
                check=True,
            )
            subprocess.run(
                [
                    "ssh",
                    f"root@{proxmox_host}",
                    f"chmod 644 {ssh_dir}/ssh_host_{key_type}_key.pub",
                ],
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
    keep_local_tarball: bool = False,
    local_tarball: Optional[Path] = None,
    compression: str = "zstd",
) -> None:
    """Provision a Proxmox LXC container.

    Args:
        machine: NixOS configuration name
        proxmox_host: Proxmox host to provision on
        bridge: Network bridge name (default: vmbr0)
        reuse_remote_tarball: Skip tarball build/copy if remote exists
        dry_run: Show what would be done without executing
        keep_local_tarball: Keep local tarball after provisioning
        local_tarball: Use existing tarball instead of building
        compression: Output compression format ('zstd' or 'xz')
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

    # Note: nesting is always enabled for LXC containers as it's required
    # for systemd 258+ credentials mechanism (sd-mkdcreds mounts ramfs)

    network_config = {
        "address": ip_alloc["address"],
        "prefix": network_info["prefix"],
        "gateway": network_info["gateway"],
        "dns": network_info.get("dns", []),
        "searchdomain": network_info.get("searchdomain"),
        "mac": ip_alloc.get("mac"),
        # Always enable nesting - required for systemd 258+ credentials mechanism
        # (sd-mkdcreds needs to mount ramfs, which requires nesting privileges)
        "features": {"nesting": True},
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

    # Determine remote tarball path
    ext = ".tar.zst" if compression == "zstd" else ".tar.xz"
    remote_tarball = f"/var/lib/vz/template/cache/proxmox-lxc-{machine}{ext}"

    # Step 3: Build/stream tarball
    should_build = True
    use_provided_tarball = local_tarball is not None
    created_local_tarball: Optional[Path] = None

    if use_provided_tarball:
        # User provided a local tarball, just copy it
        assert local_tarball is not None
        console.print("\n[bold]Step 3:[/bold] Using provided local tarball")
        console.print(f"  Tarball: {local_tarball}")
        if not local_tarball.exists():
            console.print(f"  [red]Error: Tarball not found: {local_tarball}[/red]")
            return
        should_build = False
    elif reuse_remote_tarball:
        console.print("\n[bold]Step 3:[/bold] Checking for existing remote tarball")
        if dry_run:
            console.print(
                f"  [yellow]Would check for {remote_tarball} on {proxmox_host}[/yellow]"
            )
        else:
            # Check for both .tar.zst and .tar.xz
            for check_ext in [".tar.zst", ".tar.xz"]:
                check_path = (
                    f"/var/lib/vz/template/cache/proxmox-lxc-{machine}{check_ext}"
                )
                check_cmd = [
                    "ssh",
                    f"root@{proxmox_host}",
                    f"test -f {check_path} && echo exists",
                ]
                result = subprocess.run(check_cmd, capture_output=True, text=True)
                if "exists" in result.stdout:
                    console.print(
                        f"  [green]Remote tarball exists ({check_ext}), skipping build[/green]"
                    )
                    remote_tarball = check_path
                    should_build = False
                    break

    if should_build:
        console.print(
            "\n[bold]Step 3:[/bold] Building LXC tarball and streaming to remote"
        )
        if dry_run:
            console.print(f"  [yellow]Would build: ncf build lxc {machine}[/yellow]")
            console.print(
                f"  [yellow]Would stream with secrets to {proxmox_host}:{remote_tarball}[/yellow]"
            )
        else:
            # Build the base tarball (without secrets) to temp location
            flake_ref = (
                f"{repo_root}#nixosConfigurations.{machine}.config.system.build.tarball"
            )
            with tempfile.TemporaryDirectory(prefix="ncf-lxc-build-") as temp_build_dir:
                temp_output = Path(temp_build_dir) / "result"
                build_runner = NixRunner(verbosity=1, repo_root=repo_root)
                build_runner.run_build(flake_ref, output=temp_output)

                # Find the actual tarball in the nix store result
                tarball_path = _find_tarball_in_result(temp_output)

                console.print(
                    "[bold]Streaming tarball with secrets to remote...[/bold]"
                )
                remote_tarball = stream_tarball_to_remote(
                    tarball_path, machine, proxmox_host, runner, compression
                )

    # Step 4: Copy tarball to Proxmox (if using provided tarball)
    if use_provided_tarball and not should_build:
        assert local_tarball is not None
        console.print("\n[bold]Step 4:[/bold] Copying tarball to Proxmox")
        if dry_run:
            console.print(
                f"  [yellow]Would copy {local_tarball} to {proxmox_host}:{remote_tarball}[/yellow]"
            )
        else:
            copy_tarball_to_proxmox(local_tarball, proxmox_host, machine, compression)

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
    should_cleanup_remote = should_build and not reuse_remote_tarball
    should_cleanup_local = (
        created_local_tarball is not None
        and not keep_local_tarball
        and not use_provided_tarball
    )

    if should_cleanup_remote or should_cleanup_local:
        console.print("\n[bold]Step 7:[/bold] Cleanup")
        if dry_run:
            if should_cleanup_local and created_local_tarball:
                console.print(
                    f"  [yellow]Would remove {created_local_tarball}[/yellow]"
                )
            if should_cleanup_remote:
                console.print(
                    f"  [yellow]Would remove {remote_tarball} on {proxmox_host}[/yellow]"
                )
        else:
            # Remove local tarball if created and not keeping
            if should_cleanup_local and created_local_tarball:
                if created_local_tarball.exists():
                    created_local_tarball.unlink()
                    console.print(f"  Removed local tarball: {created_local_tarball}")

            # Remove remote tarball
            if should_cleanup_remote:
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
