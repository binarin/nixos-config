"""Tests for ncf CLI functionality.

These tests verify that ncf works correctly. Environment isolation
is handled by nix (--ignore-env), not by the tests themselves.
"""

import subprocess
import sys

import pytest


class TestCliBasics:
    """Basic CLI functionality tests."""

    def test_help_works(self, run_ncf):
        """Test that ncf --help works.

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

    def test_no_crash_on_help(self, run_ncf):
        """Test that ncf doesn't crash on --help.

        Regression test for issue #130: ncf was crashing with KeyError: 'COMP_WORDS'
        when completion code was triggered but required environment variables were missing.
        """
        result = run_ncf("--help", check=False)
        assert result.returncode == 0
        assert "KeyError" not in result.stderr
        assert "COMP_WORDS" not in result.stderr

    def test_show_completion_without_crash(self, run_ncf):
        """Test that --show-completion doesn't crash."""
        # Note: --show-completion may fail or show an error message,
        # but it should not crash with a traceback
        result = run_ncf("--show-completion", check=False)
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
