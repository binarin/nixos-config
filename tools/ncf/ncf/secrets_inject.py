"""Secrets injection for NixOS deployments.

This module provides functionality to gather and decrypt secrets for injection
into LXC tarballs (and later nixos-anywhere). It queries NixOS configurations
for required secrets paths and decrypts them using sops.
"""

import json
import shutil
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

from rich.console import Console

from . import config
from .external import sops_decrypt_to_stdout, ExternalToolError
from .nix import NixRunner

console = Console()


@dataclass
class SecretFile:
    """Represents a secret file to be injected."""

    target_path: str  # Path inside the tarball/system
    source_path: Path  # Path to the sops-encrypted source file
    mode: int = 0o600  # File permissions


def get_ssh_host_keys(
    machine_name: str, runner: Optional[NixRunner] = None
) -> list[SecretFile]:
    """Get SSH host keys configuration for a machine.

    Queries nixosConfigurations.<machine>.config.services.openssh.hostKeys
    and returns the list of secret files to inject.

    Args:
        machine_name: The NixOS configuration name
        runner: Optional NixRunner instance

    Returns:
        List of SecretFile objects for SSH host keys
    """
    if runner is None:
        runner = NixRunner(verbosity=0)

    repo_root = config.find_repo_root()
    machine_secrets_dir = config.get_machine_secrets_dir(machine_name, repo_root)

    # Query the hostKeys configuration
    flake_ref = f".#nixosConfigurations.{machine_name}.config.services.openssh.hostKeys"

    try:
        result = runner.run_eval(flake_ref, json_output=True)
        host_keys = json.loads(result.stdout)
    except ExternalToolError as e:
        console.print(f"[yellow]Warning: Could not query SSH host keys: {e}[/yellow]")
        return []

    secrets = []
    for key_config in host_keys:
        target_path = key_config.get("path")
        if not target_path:
            continue

        # Derive the source file name from the target path
        # e.g., /etc/ssh/ssh_host_ed25519_key -> ssh_host_ed25519_key
        key_filename = Path(target_path).name
        source_path = machine_secrets_dir / key_filename

        if source_path.exists():
            secrets.append(
                SecretFile(
                    target_path=target_path,
                    source_path=source_path,
                    mode=0o600,
                )
            )
        else:
            console.print(f"[yellow]Warning: SSH key not found: {source_path}[/yellow]")

    return secrets


def get_user_age_key(
    machine_name: str, runner: Optional[NixRunner] = None
) -> Optional[SecretFile]:
    """Get user age key configuration for a machine.

    Queries nixosConfigurations.<machine>.config.home-manager.users.binarin.sops.age.keyFile
    and returns the secret file to inject if configured.

    Args:
        machine_name: The NixOS configuration name
        runner: Optional NixRunner instance

    Returns:
        SecretFile for user age key, or None if not configured
    """
    if runner is None:
        runner = NixRunner(verbosity=0)

    repo_root = config.find_repo_root()
    machine_secrets_dir = config.get_machine_secrets_dir(machine_name, repo_root)

    # Query the user age key configuration
    flake_ref = f".#nixosConfigurations.{machine_name}.config.home-manager.users.binarin.sops.age.keyFile"

    try:
        result = runner.run_eval(flake_ref, json_output=True)
        # Result is a JSON string like "/home/binarin/.config/sops/age/keys.txt"
        target_path = json.loads(result.stdout)
    except ExternalToolError:
        # This machine may not have home-manager or sops configured
        return None

    if not target_path:
        return None

    source_path = config.user_age_key_path(machine_secrets_dir)
    if not source_path.exists():
        console.print(
            f"[yellow]Warning: User age key not found: {source_path}[/yellow]"
        )
        return None

    return SecretFile(
        target_path=target_path,
        source_path=source_path,
        mode=0o600,
    )


def gather_secrets_for_machine(
    machine_name: str, runner: Optional[NixRunner] = None
) -> list[SecretFile]:
    """Gather all secrets that should be injected for a machine.

    Args:
        machine_name: The NixOS configuration name
        runner: Optional NixRunner instance

    Returns:
        List of all SecretFile objects to inject
    """
    secrets = []

    # SSH host keys
    secrets.extend(get_ssh_host_keys(machine_name, runner))

    # User age key
    age_key = get_user_age_key(machine_name, runner)
    if age_key:
        secrets.append(age_key)

    return secrets


def decrypt_secrets_to_tempdir(
    secrets: list[SecretFile],
) -> Path:
    """Decrypt secrets to a temporary directory.

    Creates a temporary directory with the decrypted secrets in their
    target path structure, ready for injection into a tarball.

    Args:
        secrets: List of SecretFile objects to decrypt

    Returns:
        Path to the temporary directory containing decrypted secrets.
        Caller is responsible for cleaning up this directory.
    """
    temp_dir = Path(tempfile.mkdtemp(prefix="ncf-secrets-"))

    for secret in secrets:
        # Create target directory structure
        target = temp_dir / secret.target_path.lstrip("/")
        target.parent.mkdir(parents=True, exist_ok=True)

        # Decrypt the secret
        try:
            decrypted_content = sops_decrypt_to_stdout(secret.source_path)
            target.write_text(decrypted_content)
            target.chmod(secret.mode)
        except ExternalToolError as e:
            # Clean up on failure
            shutil.rmtree(temp_dir, ignore_errors=True)
            raise ExternalToolError(
                "sops",
                f"Failed to decrypt {secret.source_path}: {e}",
            )

    return temp_dir


def generate_fake_secrets_to_tempdir(
    secrets: list[SecretFile],
) -> Path:
    """Generate fake secrets to a temporary directory.

    Creates a temporary directory with placeholder content for secrets
    in their target path structure, ready for injection into a tarball.
    Useful for testing the injection workflow without actual encrypted secrets.

    Args:
        secrets: List of SecretFile objects to generate

    Returns:
        Path to the temporary directory containing fake secrets.
        Caller is responsible for cleaning up this directory.
    """
    temp_dir = Path(tempfile.mkdtemp(prefix="ncf-secrets-fake-"))

    for secret in secrets:
        # Create target directory structure
        target = temp_dir / secret.target_path.lstrip("/")
        target.parent.mkdir(parents=True, exist_ok=True)

        # Generate placeholder content
        placeholder = f"# FAKE SECRET - placeholder for {secret.target_path}\n"
        placeholder += f"# Source: {secret.source_path}\n"
        placeholder += "# This is not a real secret, generated with --fake-secrets\n"
        target.write_text(placeholder)
        target.chmod(secret.mode)

    return temp_dir
