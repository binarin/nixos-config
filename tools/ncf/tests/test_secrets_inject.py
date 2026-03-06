"""Tests for secrets injection into LXC tarballs.

Tests verify that secrets are injected with correct file ownership.
"""

import subprocess
import tempfile
from pathlib import Path
from unittest.mock import patch

import pytest

from ncf.secrets_inject import SecretFile
from ncf.commands.build import _inject_secrets_into_tarball, _group_secrets_by_owner


class TestGroupSecretsByOwner:
    """Test grouping secrets by owner/group."""

    def test_groups_by_owner(self):
        """Test that secrets are grouped by (owner, group) tuple."""
        secrets = [
            SecretFile(
                target_path="/etc/ssh/key1",
                source_path=Path("/fake/key1"),
                owner="root",
                group="root",
            ),
            SecretFile(
                target_path="/etc/ssh/key2",
                source_path=Path("/fake/key2"),
                owner="root",
                group="root",
            ),
            SecretFile(
                target_path="/home/binarin/.config/age/keys.txt",
                source_path=Path("/fake/age"),
                owner="binarin",
                group="binarin",
            ),
        ]

        groups = _group_secrets_by_owner(secrets)

        assert len(groups) == 2
        assert ("root", "root") in groups
        assert ("binarin", "binarin") in groups
        assert len(groups[("root", "root")]) == 2
        assert len(groups[("binarin", "binarin")]) == 1


class TestSecretsInjectionOwnership:
    """Test that secrets are injected with correct ownership."""

    def test_ownership_in_tarball(self, tmp_path):
        """Test that injected secrets have correct ownership in tarball.

        This test:
        1. Creates an empty tarball
        2. Injects fake secrets with different owners
        3. Verifies ownership in the resulting tarball
        """
        # Create empty source tarball
        source_tarball = tmp_path / "source.tar.xz"
        empty_dir = tmp_path / "empty"
        empty_dir.mkdir()
        subprocess.run(
            ["tar", "-cJf", str(source_tarball), "-C", str(empty_dir), "."],
            check=True,
        )

        # Define test secrets with different owners
        test_secrets = [
            SecretFile(
                target_path="/etc/ssh/ssh_host_ed25519_key",
                source_path=Path("/fake/ssh_key"),
                mode=0o600,
                owner="root",
                group="root",
            ),
            SecretFile(
                target_path="/home/binarin/.config/age/nixos-config-keys.txt",
                source_path=Path("/fake/age_key"),
                mode=0o600,
                owner="binarin",
                group="binarin",
            ),
        ]

        output_tarball = tmp_path / "output.tar.xz"

        # Mock gather_secrets_for_machine to return our test secrets
        with patch("ncf.commands.build.gather_secrets_for_machine") as mock_gather:
            mock_gather.return_value = test_secrets

            # Run injection with fake secrets (doesn't need real sops files)
            _inject_secrets_into_tarball(
                machine_name="test-machine",
                source_tarball=source_tarball,
                output_path=output_tarball,
                runner=None,  # Not used with mocked gather
                fake_secrets=True,
            )

        # Verify tarball was created
        assert output_tarball.exists()

        # List tarball contents and check ownership
        result = subprocess.run(
            ["tar", "-tvf", str(output_tarball)],
            capture_output=True,
            text=True,
            check=True,
        )

        lines = result.stdout.strip().split("\n")
        ownership_map = {}
        for line in lines:
            # Parse tar -tv output: drwxr-xr-x root/root 0 2026-03-06 13:43 ./etc/
            parts = line.split()
            if len(parts) >= 6:
                owner_group = parts[1]  # e.g., "root/root" or "binarin/binarin"
                path = parts[-1]  # e.g., "./etc/ssh/ssh_host_ed25519_key"
                # Normalize path: remove leading ./
                path = path.lstrip("./") or "."
                ownership_map[path] = owner_group

        # Verify SSH key and its directories are owned by root
        assert ownership_map.get("etc/") == "root/root"
        assert ownership_map.get("etc/ssh/") == "root/root"
        assert ownership_map.get("etc/ssh/ssh_host_ed25519_key") == "root/root"

        # Verify age key and its directories are owned by binarin
        assert ownership_map.get("home/") == "binarin/binarin"
        assert ownership_map.get("home/binarin/") == "binarin/binarin"
        assert ownership_map.get("home/binarin/.config/") == "binarin/binarin"
        assert ownership_map.get("home/binarin/.config/age/") == "binarin/binarin"
        assert (
            ownership_map.get("home/binarin/.config/age/nixos-config-keys.txt")
            == "binarin/binarin"
        )
