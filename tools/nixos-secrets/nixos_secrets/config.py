"""Configuration and path helpers for nixos-secrets."""

from pathlib import Path
from typing import Optional


# Admin keys - these are the keys that can decrypt all secrets
ADMIN_KEYS = {
    "admin_binarin_gpg": "7668F12EF30BE2A802E1E13E6D4365C3B5E703F5",
    "admin_binarin": "age1ec9gz6f685mn8pdrz48qm4vq7qzpzcz48vgkcty9aa665evra9lq2uz3s2",
    "admin_demandred_binarin": "age1dvu92xmh3w23ghey4l0epcs27m89f3nswzs5twgnel36xxu42f4sfsv023",
}


def find_repo_root(start_path: Optional[Path] = None) -> Path:
    """Find the repository root by looking for .sops.yaml or flake.nix."""
    if start_path is None:
        start_path = Path.cwd()

    current = start_path.resolve()
    while current != current.parent:
        if (current / ".sops.yaml").exists() or (current / "flake.nix").exists():
            return current
        current = current.parent

    raise RuntimeError(
        f"Could not find repository root (no .sops.yaml or flake.nix found from {start_path})"
    )


def get_secrets_dir(repo_root: Optional[Path] = None) -> Path:
    """Get the secrets directory path."""
    if repo_root is None:
        repo_root = find_repo_root()
    return repo_root / "secrets"


def get_machine_secrets_dir(machine: str, repo_root: Optional[Path] = None) -> Path:
    """Get the secrets directory for a specific machine."""
    return get_secrets_dir(repo_root) / machine


def get_sops_yaml_path(repo_root: Optional[Path] = None) -> Path:
    """Get the .sops.yaml file path."""
    if repo_root is None:
        repo_root = find_repo_root()
    return repo_root / ".sops.yaml"


# File names within a machine's secrets directory
SSH_KEY_TYPES = ["ed25519", "rsa", "ecdsa"]


def ssh_host_key_path(machine_dir: Path, key_type: str, public: bool = False) -> Path:
    """Get path to an SSH host key file."""
    suffix = ".pub" if public else ""
    return machine_dir / f"ssh_host_{key_type}_key{suffix}"


def user_age_key_path(machine_dir: Path, public: bool = False) -> Path:
    """Get path to user age key file."""
    if public:
        return machine_dir / "user-binarin-age.pub"
    return machine_dir / "user-binarin-age"


def secrets_yaml_path(machine_dir: Path) -> Path:
    """Get path to the machine's secrets.yaml file."""
    return machine_dir / "secrets.yaml"


def user_binarin_yaml_path(machine_dir: Path) -> Path:
    """Get path to the machine's user-binarin.yaml file."""
    return machine_dir / "user-binarin.yaml"
