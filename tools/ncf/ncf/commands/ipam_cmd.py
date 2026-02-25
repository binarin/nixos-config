"""IPAM (IP Address Management) commands."""

from pathlib import Path

from rich.console import Console

from .. import config as ncf_config
from .. import ipam

console = Console()


def run_format(
    network: str | None = None,
    dry_run: bool = False,
) -> None:
    """Format network allocation files.

    Sorts IPs, converts to new hash format, and adds unallocated IP comments for /24 networks.

    Args:
        network: Specific network to format (e.g., "home"), or None for all networks
        dry_run: If True, show what would be done without making changes
    """
    repo_root = ncf_config.find_repo_root()
    networks_dir = repo_root / "inventory" / "networks"

    if network:
        network_files = [networks_dir / f"{network}.toml"]
        if not network_files[0].exists():
            console.print(f"[red]Network file not found: {network_files[0]}[/red]")
            raise SystemExit(1)
    else:
        network_files = sorted(networks_dir.glob("*.toml"))

    if dry_run:
        console.print("[yellow]DRY RUN - no changes will be made[/yellow]\n")

    for network_file in network_files:
        network_name = network_file.stem
        console.print(f"[bold]Processing {network_name}...[/bold]")

        doc = ipam.load_network_file(network_file)
        info = ipam.get_network_info(doc)

        original_content = network_file.read_text()
        formatted_doc = ipam.format_network_file(doc)
        new_content = ipam.tomlkit.dumps(formatted_doc)

        if original_content == new_content:
            console.print(f"  [dim]No changes needed[/dim]")
            continue

        # Count allocations
        ipam_data = dict(doc.get("ipam", {}))
        num_allocations = len(ipam_data)

        # Show summary
        is_24 = ipam.is_slash24_network(info)
        console.print(f"  [green]Network: {info.get('network', 'unknown')}[/green]")
        console.print(f"  [green]Allocations: {num_allocations}[/green]")
        if is_24:
            unallocated = 253 - num_allocations  # 254 usable (2-254 minus gateway)
            console.print(
                f"  [green]Unallocated: {unallocated} (will add as comments)[/green]"
            )

        if dry_run:
            console.print(f"  [yellow]Would reformat {network_file.name}[/yellow]")
        else:
            ipam.save_network_file(network_file, formatted_doc)
            console.print(f"  [green]Reformatted {network_file.name}[/green]")

    console.print("\n[bold green]Done![/bold green]")
