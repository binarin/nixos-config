"""Commands for generating files from nix configuration."""

import shutil
import subprocess
from pathlib import Path

from rich.console import Console

from ..external import register_tool

console = Console()

register_tool(
    "nix", "ncf.commands.generate", "nix build for generating ansible inventory"
)


def run_ansible_inventory(dry_run: bool = False) -> None:
    """Generate ansible inventory files from nix configuration.

    Generates two files:
    - ansible/ssh-public-keys.yaml: SSH public keys from inventory/public-keys.nix
    - ansible/ip-allocation.yaml: IP allocations from network configuration
    """
    repo_root = Path.cwd()

    # Validate we're in the repo root
    if not (repo_root / "inventory" / "public-keys.nix").exists():
        console.print(
            "[red]Error: Must be run from the repository root (inventory/public-keys.nix not found)[/red]"
        )
        raise SystemExit(1)

    # Build ssh-public-keys.yaml
    # Use flake's nixpkgs input instead of <nixpkgs> to avoid relying on NIX_PATH
    # which is not available in CI environments
    console.print("[blue]Generating ansible/ssh-public-keys.yaml...[/blue]")
    expr1 = f'let fl = builtins.getFlake (toString {repo_root}); pkgs = fl.inputs.nixpkgs.legacyPackages.${{builtins.currentSystem}}; in (pkgs.formats.yaml {{}}).generate "public-keys.yaml" (import ./inventory/public-keys.nix)'
    cmd1 = [
        "nix",
        "build",
        "--impure",
        "--print-out-paths",
        "--no-link",
        "--expr",
        expr1,
    ]
    if dry_run:
        console.print(f"[dim]Would run: {shutil.which('nix')} build --impure ...[/dim]")
        console.print(f"[dim]  Expression: {expr1}[/dim]")
    else:
        result = subprocess.run(
            cmd1, capture_output=True, text=True, check=True, cwd=repo_root
        )
        outpath = result.stdout.strip()
        dest = repo_root / "ansible" / "ssh-public-keys.yaml"
        dest.write_bytes(Path(outpath).read_bytes())
        console.print(f"[green]Generated {dest}[/green]")

    # Build ip-allocation.yaml
    # Use flake's nixpkgs input instead of <nixpkgs> to avoid relying on NIX_PATH
    console.print("[blue]Generating ansible/ip-allocation.yaml...[/blue]")
    expr2 = f"""let fl = builtins.getFlake "{repo_root}"; pkgs = fl.inputs.nixpkgs.legacyPackages.${{builtins.currentSystem}}; in (pkgs.formats.yaml {{}}).generate "ip-allocation.yaml" (let networks-lookup = import ./lib/networks-lookup.nix {{ self = fl; lib = pkgs.lib; }}; in {{ ip_allocation = networks-lookup.buildHostLookupTable (networks-lookup.readRawInventory);}})"""
    cmd2 = [
        "nix",
        "build",
        "--impure",
        "--print-out-paths",
        "--no-link",
        "--expr",
        expr2,
    ]
    if dry_run:
        console.print(f"[dim]Would run: {shutil.which('nix')} build --impure ...[/dim]")
        console.print(f"[dim]  Expression: {expr2[:80]}...[/dim]")
    else:
        result = subprocess.run(
            cmd2, capture_output=True, text=True, check=True, cwd=repo_root
        )
        outpath = result.stdout.strip()
        dest = repo_root / "ansible" / "ip-allocation.yaml"
        dest.write_bytes(Path(outpath).read_bytes())
        console.print(f"[green]Generated {dest}[/green]")

    if dry_run:
        console.print("[yellow]Dry run - no files were modified[/yellow]")
    else:
        console.print("[green]Done![/green]")
