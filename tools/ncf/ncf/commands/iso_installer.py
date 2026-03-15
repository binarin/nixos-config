"""Installer ISO operations for ncf.

This module provides functionality for building and uploading the minimal
NixOS installer ISO used with nixos-anywhere for VM provisioning.
"""

from pathlib import Path
from typing import Optional

from .. import config
from ..nix import NixRunner
from ..output import console
from ..proxmox_api import ProxmoxClient

# Default ISO filename used on Proxmox storage
ISO_FILENAME = "nixos-installer.iso"


def run_build(
    verbosity: int = 1,
    use_nom: Optional[bool] = None,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
    extra_nix_args: Optional[list[str]] = None,
) -> Path:
    """Build the minimal installer ISO.

    Args:
        verbosity: 0=quiet, 1=normal, 2=verbose
        use_nom: Use nix-output-monitor (None=auto-detect)
        builders: List of remote builders
        jobs: Number of parallel jobs
        dry_run: Show what would be done without building
        extra_nix_args: Extra arguments to pass to nix build

    Returns:
        Path to the built ISO file in the Nix store
    """
    repo_root = config.find_repo_root()

    flake_ref = f"{repo_root}#nixosConfigurations.iso.config.system.build.isoImage"

    if dry_run:
        console.print(f"[bold]Would build:[/bold] {flake_ref}")
        if builders:
            console.print(f"[bold]Builders:[/bold] {', '.join(builders)}")
        if extra_nix_args:
            console.print(f"[bold]Extra nix args:[/bold] {' '.join(extra_nix_args)}")
        return Path("/dry-run/nixos-installer.iso")

    console.print("[bold]Building installer ISO[/bold]")

    runner = NixRunner(
        verbosity=verbosity,
        use_nom=use_nom,
        builders=builders or [],
        keep_going=True,
        jobs=jobs,
        repo_root=repo_root,
    )

    import tempfile

    with tempfile.TemporaryDirectory() as tmpdir:
        result_link = Path(tmpdir) / "result"
        runner.run_build(flake_ref, output=result_link, extra_args=extra_nix_args)

        # Find the actual ISO in the build result
        iso_files = list(result_link.rglob("*.iso"))
        if not iso_files:
            raise RuntimeError("No ISO file found in build result")

        built_iso = iso_files[0].resolve()

    console.print(f"[green]✓[/green] Built installer ISO: {built_iso}")
    return built_iso


def run_upload(
    proxmox_host: str,
    storage: str = "local",
    iso_path: Optional[Path] = None,
    force: bool = False,
    dry_run: bool = False,
) -> None:
    """Upload installer ISO to Proxmox.

    Args:
        proxmox_host: Proxmox host to upload to
        storage: Storage name (must have iso content type)
        iso_path: Path to local ISO (None = build first)
        force: Upload even if ISO already exists on Proxmox
        dry_run: Show what would be done without uploading
    """
    if iso_path is None:
        iso_path = run_build()

    if not iso_path.exists():
        console.print(f"[red]Error: ISO not found at {iso_path}[/red]")
        console.print("Build it first with: ncf build iso-installer")
        raise SystemExit(1)

    if dry_run:
        console.print(f"[bold]Would upload:[/bold] {iso_path}")
        console.print(f"[bold]To:[/bold] {proxmox_host}:{storage}")
        return

    console.print(f"[bold]Uploading installer ISO to {proxmox_host}[/bold]")

    client = ProxmoxClient(proxmox_host)

    # Check if ISO already exists
    if client.iso_exists(storage, ISO_FILENAME):
        if force:
            console.print(f"  [yellow]ISO '{ISO_FILENAME}' exists, overwriting[/yellow]")
        else:
            console.print(f"  [green]✓[/green] ISO '{ISO_FILENAME}' already exists")
            return

    console.print(f"  Uploading {iso_path.name}...")
    client.upload_iso(storage, iso_path, filename=ISO_FILENAME)
    console.print(f"  [green]✓[/green] ISO uploaded to {storage}:iso/{ISO_FILENAME}")


def ensure_iso_on_proxmox(
    proxmox_host: str,
    storage: str = "local",
    force: bool = False,
    verbosity: int = 1,
    dry_run: bool = False,
) -> str:
    """Ensure installer ISO is available on Proxmox.

    Checks if the ISO exists on Proxmox, and if not, builds and uploads it.

    Args:
        proxmox_host: Proxmox host
        storage: Storage name
        force: Rebuild and re-upload even if ISO exists
        verbosity: Verbosity level
        dry_run: Show what would be done

    Returns:
        ISO path in Proxmox format (e.g., "local:iso/nixos-installer.iso")
    """
    iso_ref = f"{storage}:iso/{ISO_FILENAME}"

    if dry_run:
        console.print(f"[bold]Would ensure ISO at:[/bold] {iso_ref}")
        return iso_ref

    client = ProxmoxClient(proxmox_host)

    # Check if already on Proxmox
    if not force and client.iso_exists(storage, ISO_FILENAME):
        console.print(f"  [green]✓[/green] ISO already present: {ISO_FILENAME}")
        return iso_ref

    # Build and upload
    console.print("  Building installer ISO...")
    iso_path = run_build(verbosity=verbosity)
    console.print(f"  Uploading ISO to {proxmox_host}...")
    client.upload_iso(storage, iso_path, filename=ISO_FILENAME)
    console.print(f"  [green]✓[/green] ISO uploaded: {iso_ref}")

    return iso_ref
