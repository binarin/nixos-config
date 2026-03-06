"""CI utilities for ncf."""

import base64
import json
from pathlib import Path

from rich.console import Console

import shutil

from rich.table import Table

from .. import config
from ..external import run_command, register_tool, get_registered_tools

console = Console()

# Register tools used by CI commands
register_tool("git", "Git operations for repository management")
register_tool("git-crypt", "List encrypted files for CI secrets handling")


def get_git_crypt_encrypted_files() -> list[Path]:
    """Get list of git-crypt encrypted files."""
    repo_root = config.find_repo_root()
    result = run_command(["git", "crypt", "status", "-e"], cwd=repo_root)
    files = []
    for line in result.stdout.strip().split("\n"):
        if line.strip():
            # Format is "    encrypted: path/to/file"
            parts = line.split(":", 1)
            if len(parts) == 2:
                filepath = parts[1].strip()
                files.append(repo_root / filepath)
    return files


def generate_fake_jwe() -> str:
    """Generate a valid-looking JWE compact serialization placeholder.

    JWE format: header.encrypted_key.iv.ciphertext.tag
    All parts are base64url encoded.
    """
    # Header: {"alg":"dir","enc":"A256GCM"} - direct encryption with AES-256-GCM
    header = (
        base64.urlsafe_b64encode(b'{"alg":"dir","enc":"A256GCM"}').decode().rstrip("=")
    )
    # For direct encryption, encrypted_key is empty
    encrypted_key = ""
    # IV: 12 bytes for AES-GCM, use zeros
    iv = base64.urlsafe_b64encode(b"\x00" * 12).decode().rstrip("=")
    # Ciphertext: some placeholder bytes
    ciphertext = (
        base64.urlsafe_b64encode(b"PLACEHOLDER_ENCRYPTED_DATA").decode().rstrip("=")
    )
    # Auth tag: 16 bytes for AES-GCM
    tag = base64.urlsafe_b64encode(b"\x00" * 16).decode().rstrip("=")

    return f"{header}.{encrypted_key}.{iv}.{ciphertext}.{tag}"


def generate_fake_content_for_file(filepath: Path) -> str:
    """Generate appropriate placeholder content based on file extension."""
    suffix = filepath.suffix.lower()
    name = filepath.name.lower()

    if suffix == ".jwe":
        return generate_fake_jwe()
    elif suffix == ".json":
        return "{}"
    elif ".git-crypt" in name or suffix == ".git-crypt":
        return "PLACEHOLDER_SECRET\n"
    else:
        return f"# Placeholder for {filepath.name}\n"


def run_fake_unlock() -> None:
    """Replace git-crypt encrypted files with placeholder content.

    This allows CI builds to proceed without real secrets.
    """
    encrypted_files = get_git_crypt_encrypted_files()

    if not encrypted_files:
        console.print("[yellow]No git-crypt encrypted files found[/yellow]")
        return

    console.print(
        f"[bold]Replacing {len(encrypted_files)} encrypted files with placeholders...[/bold]"
    )

    for filepath in encrypted_files:
        if not filepath.exists():
            console.print(f"[yellow]Skipping missing file: {filepath}[/yellow]")
            continue

        fake_content = generate_fake_content_for_file(filepath)
        filepath.write_text(fake_content)
        console.print(
            f"[green]✓[/green] {filepath.relative_to(config.find_repo_root())}"
        )

    console.print("\n[bold green]Fake unlock complete![/bold green]")


def get_nixos_configurations() -> list[str]:
    """Get list of all nixosConfigurations from the flake."""
    repo_root = config.find_repo_root()
    result = run_command(
        [
            "nix",
            "eval",
            "--json",
            ".#nixosConfigurations",
            "--apply",
            "builtins.attrNames",
        ],
        cwd=repo_root,
    )
    return json.loads(result.stdout)


def get_ci_config(configuration: str) -> dict:
    """Get CI configuration for a nixosConfiguration."""
    repo_root = config.find_repo_root()
    result = run_command(
        [
            "nix",
            "eval",
            "--json",
            f".#nixosConfigurations.{configuration}.config.ci",
        ],
        cwd=repo_root,
    )
    return json.loads(result.stdout)


def get_deploy_nodes() -> list[str]:
    """Get list of machines that have deploy-rs nodes defined."""
    repo_root = config.find_repo_root()
    result = run_command(
        [
            "nix",
            "eval",
            "--json",
            ".#deploy.nodes",
            "--apply",
            "builtins.attrNames",
        ],
        cwd=repo_root,
    )
    return json.loads(result.stdout)


def get_configurations_to_build() -> list[str]:
    """Get list of configuration names that should be built by CI."""
    configs = get_nixos_configurations()
    result = []
    for cfg in configs:
        try:
            ci_config = get_ci_config(cfg)
            if ci_config.get("doBuild", True):
                result.append(cfg)
        except Exception as e:
            # If we can't get CI config, include it by default
            console.print(
                f"[yellow]Warning: Could not get CI config for {cfg}: {e}[/yellow]"
            )
            result.append(cfg)
    return result


def get_build_path(name: str) -> str:
    """Get the nix build path for a configuration.

    Returns deploy-rs path if available, otherwise nixosConfiguration toplevel.
    """
    deploy_nodes = get_deploy_nodes()
    if name in deploy_nodes:
        return f".#deploy.nodes.{name}.profiles.system.path"
    else:
        return f".#nixosConfigurations.{name}.config.system.build.toplevel"


def run_matrix() -> None:
    """Output JSON array of configuration names for CI dynamic matrix."""
    configurations = get_configurations_to_build()
    print(json.dumps(configurations))


def run_build_path(name: str) -> None:
    """Output the nix build path for a configuration."""
    print(get_build_path(name))


def run_external_deps() -> None:
    """Display all registered external tool dependencies."""
    # Import all command modules to ensure their tool registrations are executed
    from . import (  # noqa: F401
        add_machine,
        build,
        eval,
        init_machine,
        ipam_cmd,
        iso,
        list_machines,
        provision_lxc,
        set_secret,
        verify,
    )
    from .. import nix  # noqa: F401

    tools = get_registered_tools()

    if not tools:
        console.print("[yellow]No external tools registered[/yellow]")
        return

    # Sort tools: required first, then alphabetically
    sorted_tools = sorted(tools.values(), key=lambda t: (not t.required, t.name))

    table = Table(title="External Tool Dependencies")
    table.add_column("Tool", style="cyan")
    table.add_column("Status", style="bold")
    table.add_column("Required", style="magenta")
    table.add_column("Module", style="blue")
    table.add_column("Purpose", style="white")

    for tool in sorted_tools:
        # Check if tool is available
        available = shutil.which(tool.name) is not None
        status = "[green]✓[/green]" if available else "[red]✗[/red]"
        required = "Yes" if tool.required else "No"

        # Add a row for each registration
        for i, reg in enumerate(tool.registrations):
            if i == 0:
                table.add_row(tool.name, status, required, reg.module, reg.purpose)
            else:
                table.add_row("", "", "", reg.module, reg.purpose)

    console.print(table)

    # Summary
    required_tools = [t for t in tools.values() if t.required]
    optional_tools = [t for t in tools.values() if not t.required]
    missing_required = [t for t in required_tools if not shutil.which(t.name)]
    missing_optional = [t for t in optional_tools if not shutil.which(t.name)]

    console.print()
    if missing_required:
        console.print(
            f"[red]Missing required tools: {', '.join(t.name for t in missing_required)}[/red]"
        )
    if missing_optional:
        console.print(
            f"[yellow]Missing optional tools: {', '.join(t.name for t in missing_optional)}[/yellow]"
        )
    if not missing_required and not missing_optional:
        console.print("[green]All external tools are available![/green]")
