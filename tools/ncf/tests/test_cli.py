"""Tests for ncf CLI functionality.

These tests verify that ncf works correctly in various environments,
including when run with minimal environment variables (as with `nix run --ignore-env`).
"""

import os
import subprocess
import sys

import pytest


class TestCliBasics:
    """Basic CLI functionality tests."""

    def test_help_works_in_clean_env(self, run_ncf):
        """Test that ncf --help works in a clean environment.

        This is the basic smoke test - if --help fails, something is fundamentally broken.
        """
        result = run_ncf("--help", check=False)
        assert result.returncode == 0
        assert "CLI tool for NixOS configuration management" in result.stdout

    def test_no_args_shows_help(self, run_ncf):
        """Test that running ncf without arguments shows help."""
        result = run_ncf(check=False)
        # typer with no_args_is_help=True exits with code 2 when no args given
        assert result.returncode == 2
        assert "Usage:" in result.stdout

    def test_subcommand_help(self, run_ncf):
        """Test that subcommand help works."""
        result = run_ncf("secrets", "--help", check=False)
        assert result.returncode == 0
        assert "Manage NixOS secrets" in result.stdout


class TestCompletionHandling:
    """Tests for shell completion handling.

    These tests verify that completion-related code doesn't crash when
    environment variables are missing or in unexpected states.
    """

    def test_no_crash_with_empty_env(self, clean_env):
        """Test that ncf doesn't crash with minimal environment.

        Regression test for issue #130: ncf was crashing with KeyError: 'COMP_WORDS'
        when completion code was triggered but required environment variables were missing.
        """
        result = subprocess.run(
            [sys.executable, "-m", "ncf", "--help"],
            env=clean_env,
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0
        assert "KeyError" not in result.stderr
        assert "COMP_WORDS" not in result.stderr

    def test_no_crash_with_partial_completion_env(self, clean_env):
        """Test that ncf handles partial completion environment gracefully.

        When _NCF_COMPLETE is set but COMP_WORDS is missing, ncf should
        either complete gracefully or show normal output, not crash.
        """
        env = clean_env.copy()
        # Set the completion trigger without the required variables
        env["_NCF_COMPLETE"] = "complete"

        result = subprocess.run(
            [sys.executable, "-m", "ncf"],
            env=env,
            capture_output=True,
            text=True,
        )

        # Should not crash with KeyError
        assert "KeyError" not in result.stderr
        assert "COMP_WORDS" not in result.stderr
        # Exit code may be non-zero if completion fails gracefully, but shouldn't crash

    def test_show_completion_without_crash(self, clean_env):
        """Test that --show-completion doesn't crash in clean environment."""
        # Note: --show-completion may fail or show an error message,
        # but it should not crash with a traceback
        result = subprocess.run(
            [sys.executable, "-m", "ncf", "--show-completion"],
            env=clean_env,
            capture_output=True,
            text=True,
        )
        # Should not have Python traceback
        assert "Traceback (most recent call last)" not in result.stderr


class TestSubcommands:
    """Test that various subcommands are accessible."""

    @pytest.mark.parametrize(
        "subcommand",
        [
            "secrets",
            "ci",
            "iso",
            "build",
            "eval",
            "machine",
            "ipam",
        ],
    )
    def test_subcommand_accessible(self, run_ncf, subcommand):
        """Test that each subcommand is accessible and shows help."""
        result = run_ncf(subcommand, "--help", check=False)
        assert result.returncode == 0
        assert "Usage:" in result.stdout
