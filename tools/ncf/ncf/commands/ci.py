"""CI utilities for ncf."""

import base64
import json
from pathlib import Path

import shutil

from rich.table import Table

from .. import config
from ..external import run_command, register_tool, get_registered_tools
from ..output import console

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


def get_deploy_profiles() -> list[tuple[str, str]]:
    """Get list of all deploy profiles as (node, profile) tuples."""
    repo_root = config.find_repo_root()
    result = run_command(
        [
            "nix",
            "eval",
            "--json",
            ".#deploy.nodes",
            "--apply",
            """
            nodes: builtins.concatLists (
              builtins.attrValues (
                builtins.mapAttrs (nodeName: node:
                  map (profileName: { inherit nodeName profileName; })
                      (builtins.attrNames (node.profiles or {}))
                ) nodes
              )
            )
            """,
        ],
        cwd=repo_root,
    )
    profiles = json.loads(result.stdout)
    return [(p["nodeName"], p["profileName"]) for p in profiles]


def get_packages(system: str = "x86_64-linux") -> list[str]:
    """Get list of all packages from the flake for a given system."""
    repo_root = config.find_repo_root()
    result = run_command(
        [
            "nix",
            "eval",
            "--json",
            f".#packages.{system}",
            "--apply",
            "builtins.attrNames",
        ],
        cwd=repo_root,
    )
    return json.loads(result.stdout)


def get_devshells(system: str = "x86_64-linux") -> list[str]:
    """Get list of all devShells from the flake for a given system."""
    repo_root = config.find_repo_root()
    result = run_command(
        [
            "nix",
            "eval",
            "--json",
            f".#devShells.{system}",
            "--apply",
            "builtins.attrNames",
        ],
        cwd=repo_root,
    )
    return json.loads(result.stdout)


def get_checks(system: str = "x86_64-linux") -> list[str]:
    """Get list of all checks from the flake for a given system."""
    repo_root = config.find_repo_root()
    result = run_command(
        [
            "nix",
            "eval",
            "--json",
            f".#checks.{system}",
            "--apply",
            "builtins.attrNames",
        ],
        cwd=repo_root,
    )
    return json.loads(result.stdout)


def get_home_configurations() -> list[str]:
    """Get list of all homeConfigurations from the flake."""
    repo_root = config.find_repo_root()
    result = run_command(
        [
            "nix",
            "eval",
            "--json",
            ".#homeConfigurations",
            "--apply",
            "builtins.attrNames",
        ],
        cwd=repo_root,
    )
    return json.loads(result.stdout)


def get_matrix_entries() -> list[str]:
    """Get list of prefixed matrix entries for CI.

    Returns entries with prefixes:
    - d:<profile>:<node> for deploy-rs profiles (e.g., d:system:acme, d:user:b-adb-k)
    - c:<name> for nixosConfigurations not in any deploy node
    - h:<name> for homeConfigurations (standalone builds)
    - p:<name> for x86_64-linux packages
    - s:<name> for x86_64-linux devshells
    - k:<name> for x86_64-linux checks
    """
    result = []

    # Get all deploy profiles
    try:
        deploy_profiles = get_deploy_profiles()
        deployed_nodes = {node for node, _ in deploy_profiles}
        for node, profile in deploy_profiles:
            result.append(f"d:{profile}:{node}")
    except Exception as e:
        console.print(f"[yellow]Warning: Could not get deploy profiles: {e}[/yellow]")
        deployed_nodes = set()

    # Get NixOS configurations not covered by deploy nodes
    configs = get_nixos_configurations()
    for cfg in configs:
        if cfg in deployed_nodes:
            continue
        try:
            ci_config = get_ci_config(cfg)
            if ci_config.get("doBuild", True):
                result.append(f"c:{cfg}")
        except Exception as e:
            # If we can't get CI config, include it by default
            console.print(
                f"[yellow]Warning: Could not get CI config for {cfg}: {e}[/yellow]"
            )
            result.append(f"c:{cfg}")

    # Get homeConfigurations (standalone builds)
    try:
        home_configs = get_home_configurations()
        for cfg in home_configs:
            result.append(f"h:{cfg}")
    except Exception as e:
        console.print(
            f"[yellow]Warning: Could not get homeConfigurations: {e}[/yellow]"
        )

    # Get packages for x86_64-linux
    try:
        packages = get_packages("x86_64-linux")
        for pkg in packages:
            result.append(f"p:{pkg}")
    except Exception as e:
        console.print(f"[yellow]Warning: Could not get packages: {e}[/yellow]")

    # Get devShells for x86_64-linux
    try:
        devshells = get_devshells("x86_64-linux")
        for shell in devshells:
            result.append(f"s:{shell}")
    except Exception as e:
        console.print(f"[yellow]Warning: Could not get devShells: {e}[/yellow]")

    # Get checks for x86_64-linux
    try:
        checks = get_checks("x86_64-linux")
        for check in checks:
            result.append(f"k:{check}")
    except Exception as e:
        console.print(f"[yellow]Warning: Could not get checks: {e}[/yellow]")

    return result


def get_build_path(entry: str) -> str:
    """Get the nix build path for a matrix entry.

    Handles prefixed entries:
    - d:<profile>:<node> -> deploy-rs profile path (e.g., d:system:acme, d:user:b-adb-k)
    - c:<name> -> nixosConfiguration toplevel
    - h:<name> -> homeConfiguration activationPackage
    - p:<name> -> package for x86_64-linux
    - s:<name> -> devShell for x86_64-linux
    - k:<name> -> check for x86_64-linux
    - <name> (no prefix) -> legacy behavior, check deploy nodes
    """
    if ":" in entry:
        parts = entry.split(":", 2)
        prefix = parts[0]
        if prefix == "d":
            if len(parts) != 3:
                raise ValueError(
                    f"Invalid deploy entry format: {entry}, expected d:<profile>:<node>"
                )
            profile, node = parts[1], parts[2]
            return f".#deploy.nodes.{node}.profiles.{profile}.path"
        elif prefix == "c":
            name = parts[1]
            return f".#nixosConfigurations.{name}.config.system.build.toplevel"
        elif prefix == "h":
            name = parts[1]
            return f".#homeConfigurations.{name}.activationPackage"
        elif prefix == "p":
            name = parts[1]
            return f".#packages.x86_64-linux.{name}"
        elif prefix == "s":
            name = parts[1]
            return f".#devShells.x86_64-linux.{name}"
        elif prefix == "k":
            name = parts[1]
            return f".#checks.x86_64-linux.{name}"
        else:
            raise ValueError(f"Unknown prefix: {prefix}")
    else:
        # Legacy behavior for backwards compatibility
        deploy_nodes = get_deploy_nodes()
        if entry in deploy_nodes:
            return f".#deploy.nodes.{entry}.profiles.system.path"
        else:
            return f".#nixosConfigurations.{entry}.config.system.build.toplevel"


def expand_matrix_entry(entry: str) -> str:
    """Expand a matrix entry prefix to a human-readable name.

    Expands prefixes to descriptive names:
    - d:<profile>:<node> -> deploy-<profile>-<node>
    - c:<name> -> nixos-config-<name>
    - h:<name> -> home-config-<name>
    - p:<name> -> package-<name>
    - s:<name> -> devshell-<name>
    - k:<name> -> check-<name>
    """
    if ":" in entry:
        parts = entry.split(":", 2)
        prefix = parts[0]
        if prefix == "d":
            if len(parts) != 3:
                raise ValueError(f"Invalid deploy entry format: {entry}")
            profile, node = parts[1], parts[2]
            return f"deploy-{profile}-{node}"
        elif prefix == "c":
            return f"nixos-config-{parts[1]}"
        elif prefix == "h":
            return f"home-config-{parts[1]}"
        elif prefix == "p":
            return f"package-{parts[1]}"
        elif prefix == "s":
            return f"devshell-{parts[1]}"
        elif prefix == "k":
            return f"check-{parts[1]}"
        else:
            raise ValueError(f"Unknown prefix: {prefix}")
    else:
        return entry


def run_matrix() -> None:
    """Output JSON array of matrix entries for CI dynamic matrix."""
    entries = get_matrix_entries()
    print(json.dumps(entries))


def run_build_path(entry: str) -> None:
    """Output the nix build path for a matrix entry."""
    print(get_build_path(entry))


def run_expand_matrix_entry(entry: str) -> None:
    """Output the expanded name for a matrix entry."""
    print(expand_matrix_entry(entry))


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
