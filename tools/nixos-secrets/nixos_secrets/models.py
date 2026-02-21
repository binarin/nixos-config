"""Data models for nixos-secrets."""

from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

from . import config


@dataclass
class KeyInfo:
    """Information about a cryptographic key."""

    public_key: Optional[str] = None
    exists: bool = False
    encrypted: bool = False


@dataclass
class MachineSecrets:
    """Represents the secrets state for a single machine."""

    name: str
    directory: Path
    ssh_keys: dict[str, KeyInfo] = field(default_factory=dict)
    user_age_key: KeyInfo = field(default_factory=KeyInfo)
    secrets_yaml_exists: bool = False
    user_binarin_yaml_exists: bool = False
    in_sops_yaml: bool = False

    @classmethod
    def from_directory(cls, machine_dir: Path) -> "MachineSecrets":
        """Create a MachineSecrets instance by scanning a directory."""
        machine = cls(
            name=machine_dir.name,
            directory=machine_dir,
        )

        # Check SSH keys
        for key_type in config.SSH_KEY_TYPES:
            key_path = config.ssh_host_key_path(machine_dir, key_type)
            pub_path = config.ssh_host_key_path(machine_dir, key_type, public=True)

            key_info = KeyInfo()
            if key_path.exists():
                key_info.exists = True
                # Check if encrypted by looking for sops metadata
                content = key_path.read_text()
                key_info.encrypted = "sops" in content and "ENC[" in content

            if pub_path.exists():
                key_info.public_key = pub_path.read_text().strip()

            machine.ssh_keys[key_type] = key_info

        # Check user age key
        age_key_path = config.user_age_key_path(machine_dir)
        age_pub_path = config.user_age_key_path(machine_dir, public=True)

        if age_key_path.exists():
            machine.user_age_key.exists = True
            content = age_key_path.read_text()
            machine.user_age_key.encrypted = "sops" in content and "ENC[" in content

        if age_pub_path.exists():
            machine.user_age_key.public_key = age_pub_path.read_text().strip()

        # Check YAML files
        machine.secrets_yaml_exists = config.secrets_yaml_path(machine_dir).exists()
        machine.user_binarin_yaml_exists = config.user_binarin_yaml_path(
            machine_dir
        ).exists()

        return machine

    @property
    def has_all_ssh_keys(self) -> bool:
        """Check if all SSH key types exist."""
        return all(
            key_info.exists for key_info in self.ssh_keys.values() if self.ssh_keys
        )

    @property
    def all_ssh_keys_encrypted(self) -> bool:
        """Check if all existing SSH keys are encrypted."""
        return all(
            key_info.encrypted for key_info in self.ssh_keys.values() if key_info.exists
        )

    @property
    def has_user_key(self) -> bool:
        """Check if user age key exists."""
        return self.user_age_key.exists

    @property
    def user_key_encrypted(self) -> bool:
        """Check if user age key is encrypted."""
        return self.user_age_key.encrypted

    @property
    def is_complete(self) -> bool:
        """Check if the machine has all required secrets configured."""
        return (
            self.has_all_ssh_keys
            and self.all_ssh_keys_encrypted
            and self.has_user_key
            and self.user_key_encrypted
            and self.secrets_yaml_exists
            and self.user_binarin_yaml_exists
            and self.in_sops_yaml
        )
