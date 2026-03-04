"""CI utilities for ncf."""

import base64
import json
from pathlib import Path

from rich.console import Console

from .. import config
from ..external import run_command

console = Console()


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


def get_configurations_to_build() -> list[dict]:
    """Get list of configurations that should be built by CI with their build paths."""
    configs = get_nixos_configurations()
    deploy_nodes = get_deploy_nodes()
    result = []
    for cfg in configs:
        try:
            ci_config = get_ci_config(cfg)
            if ci_config.get("doBuild", True):
                # Check if machine has deploy-rs node defined
                if cfg in deploy_nodes:
                    build_path = f".#deploy.nodes.{cfg}.profiles.system.path"
                else:
                    build_path = (
                        f".#nixosConfigurations.{cfg}.config.system.build.toplevel"
                    )
                result.append({"name": cfg, "buildPath": build_path})
        except Exception as e:
            # If we can't get CI config, include it by default
            console.print(
                f"[yellow]Warning: Could not get CI config for {cfg}: {e}[/yellow]"
            )
            if cfg in deploy_nodes:
                build_path = f".#deploy.nodes.{cfg}.profiles.system.path"
            else:
                build_path = f".#nixosConfigurations.{cfg}.config.system.build.toplevel"
            result.append({"name": cfg, "buildPath": build_path})
    return result


def run_matrix() -> None:
    """Output JSON array of JSON strings for CI dynamic matrix.

    Each configuration is output as a JSON string to work around Forgejo's
    lack of support for object property access in matrix expressions.
    """
    configurations = get_configurations_to_build()
    # Output array of JSON strings - each config is stringified
    json_strings = [json.dumps(c) for c in configurations]
    print(json.dumps(json_strings))
