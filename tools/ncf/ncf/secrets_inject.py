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

from . import config
from .external import sops_decrypt_to_stdout, ExternalToolError
from .nix import NixRunner
from .output import console


@dataclass
class SecretFile:
    """Represents a secret file to be injected."""

    target_path: str  # Path inside the tarball/system
    source_path: Path  # Path to the sops-encrypted source file (or decrypted if pre_decrypted)
    mode: int = 0o600  # File permissions
    owner: str = "root"  # File owner
    group: str = "root"  # File group
    pre_decrypted: bool = False  # If True, source_path contains already-decrypted content


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

    # User secrets should be owned by the user, not root
    return SecretFile(
        target_path=target_path,
        source_path=source_path,
        mode=0o600,
        owner="binarin",
        group="binarin",
    )


def is_clan_machine(machine_name: str) -> bool:
    """Check if a machine is managed by clan.

    Uses clan_lib to list clan-managed machines and checks if the given
    machine is among them.
    """
    try:
        from clan_lib.flake import Flake
        from clan_lib.machines.actions import list_machines
    except ImportError:
        return False

    repo_root = config.find_repo_root()
    flake = Flake(str(repo_root))
    machines = list_machines(flake)
    return machine_name in machines


def gather_secrets_for_clan_machine(machine_name: str) -> list[SecretFile]:
    """Gather secrets for a clan-managed machine using clan_lib.

    Uses clan's SecretStore.populate_dir() to stage all secrets (age key +
    activation secrets) into a temp directory, then converts them to SecretFile
    objects with pre_decrypted=True.

    The caller is responsible for cleaning up source files after use (they
    live in a temp directory).
    """
    from clan_lib.flake import Flake
    from clan_cli.vars.secret_modules.sops import SecretStore

    repo_root = config.find_repo_root()
    flake = Flake(str(repo_root))
    secret_store = SecretStore(flake)

    upload_dir = secret_store.get_upload_directory(machine_name)

    # Stage secrets to a temp directory
    temp_dir = Path(tempfile.mkdtemp(prefix="ncf-clan-secrets-"))
    secret_store.populate_dir(
        machine_name,
        temp_dir,
        phases=["activation", "users", "services"],
    )

    # Walk the output and create SecretFile objects
    secrets = []
    for path in temp_dir.rglob("*"):
        if path.is_file():
            rel_path = path.relative_to(temp_dir)
            target_path = f"{upload_dir}/{rel_path}"
            secrets.append(
                SecretFile(
                    target_path=target_path,
                    source_path=path,
                    mode=path.stat().st_mode & 0o777,
                    pre_decrypted=True,
                )
            )

    if secrets:
        console.print(f"  Gathered {len(secrets)} clan secret(s) for {machine_name}")
    else:
        console.print(f"  [yellow]No clan secrets found for {machine_name}[/yellow]")

    return secrets


def gather_secrets_for_machine(
    machine_name: str, runner: Optional[NixRunner] = None
) -> list[SecretFile]:
    """Gather all secrets that should be injected for a machine.

    For clan-managed machines, uses clan_lib's SecretStore to gather secrets.
    For non-clan machines, injects SSH host keys only (user age key is
    decrypted at runtime by sops-nix using the SSH host key).

    Args:
        machine_name: The NixOS configuration name
        runner: Optional NixRunner instance

    Returns:
        List of all SecretFile objects to inject
    """
    if is_clan_machine(machine_name):
        console.print(f"  [cyan]Detected clan-managed machine: {machine_name}[/cyan]")
        return gather_secrets_for_clan_machine(machine_name)

    # Legacy path: only inject SSH host keys
    return get_ssh_host_keys(machine_name, runner)


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

        if secret.pre_decrypted:
            # Already decrypted (e.g., by clan_lib), just copy
            shutil.copy2(secret.source_path, target)
            target.chmod(secret.mode)
        else:
            # Decrypt the secret using sops
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
