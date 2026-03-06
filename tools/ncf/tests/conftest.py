"""Pytest configuration and fixtures for ncf tests."""

import subprocess
import sys

import pytest


@pytest.fixture
def run_ncf():
    """Fixture to run ncf commands.

    Environment isolation is handled by nix (--ignore-env), not Python.
    """

    def _run_ncf(*args, check=True, capture_output=True):
        cmd = [sys.executable, "-m", "ncf"] + list(args)
        return subprocess.run(
            cmd,
            check=check,
            capture_output=capture_output,
            text=True,
        )

    return _run_ncf
