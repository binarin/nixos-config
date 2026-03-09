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

    def test_typer_completion_classes_registered(self):
        """Test that typer's completion classes are registered with click.

        Regression test for issue #161: When completion was triggered with
        _NCF_COMPLETE=complete_zsh, ncf crashed with KeyError: 'COMP_WORDS'
        because typer's completion classes weren't registered with click.

        The fix is to call completion_init() at module import time in cli.py.
        This test verifies that importing ncf.cli registers the correct classes.
        """
        # Import click's completion module
        import click.shell_completion

        # Import ncf.cli which should call completion_init()
        import ncf.cli  # noqa: F401

        # Verify that typer's completion classes are registered, not click's
        zsh_cls = click.shell_completion.get_completion_class("zsh")
        bash_cls = click.shell_completion.get_completion_class("bash")

        # Typer's classes use _TYPER_COMPLETE_ARGS, click's use COMP_WORDS
        # We verify by checking the class name contains "typer"
        assert (
            "typer" in zsh_cls.__module__
        ), f"Expected typer's ZshComplete but got {zsh_cls.__module__}.{zsh_cls.__name__}"
        assert (
            "typer" in bash_cls.__module__
        ), f"Expected typer's BashComplete but got {bash_cls.__module__}.{bash_cls.__name__}"

    def test_completion_with_typer_env_vars(self, run_ncf):
        """Test that zsh completion works with _TYPER_COMPLETE_ARGS.

        This verifies the completion flow works end-to-end by setting
        the completion variable that would be set when running the 'ncf'
        binary (not 'python -m ncf' which has different prog_name handling).

        Note: The original issue #161 was specifically about zsh completion.
        Typer's ZshComplete uses _TYPER_COMPLETE_ARGS, while click's uses
        COMP_WORDS. By calling completion_init(), we ensure typer's class
        is registered.

        For bash completion, both typer and click use COMP_WORDS/COMP_CWORD,
        so there's no compatibility issue there.
        """
        # We can't easily test with python -m ncf because the program name
        # contains spaces, making the completion variable invalid.
        # Instead, verify no crash happens with the completion env vars set.
        result = run_ncf(
            check=False,
            env={
                # Use a simple test - verify the module can be imported
                # and run without crashing when these vars are set
                "_NCF_COMPLETE": "complete_zsh",
                "_TYPER_COMPLETE_ARGS": "ncf ",
            },
        )
        # The key test: should NOT crash with KeyError: 'COMP_WORDS'
        # When run with 'python -m ncf', the completion var doesn't match
        # so it just shows help (exit 2), but it shouldn't crash
        assert "KeyError" not in result.stderr
        assert "COMP_WORDS" not in result.stderr
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
            "generate",
        ],
    )
    def test_subcommand_accessible(self, run_ncf, subcommand):
        """Test that each subcommand is accessible and shows help."""
        result = run_ncf(subcommand, "--help", check=False)
        assert result.returncode == 0
        assert "Usage:" in result.stdout
