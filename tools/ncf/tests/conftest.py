"""Pytest configuration and fixtures for ncf tests."""

import os
import subprocess
import sys
from pathlib import Path

import pytest


@pytest.fixture
def ncf_path():
    """Return the path to the ncf executable."""
    # When running under nix, ncf should be in PATH
    # When running in development, use the module directly
    return sys.executable


@pytest.fixture
def clean_env():
    """Return a minimal clean environment for subprocess calls.

    This simulates running with `nix run --ignore-env` by providing
    only essential environment variables.
    """
    return {
        "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
        "HOME": os.environ.get("HOME", "/tmp"),
        "TERM": os.environ.get("TERM", "xterm"),
    }


@pytest.fixture
def run_ncf(clean_env):
    """Fixture to run ncf commands in a clean environment."""

    def _run_ncf(*args, env=None, check=True, capture_output=True):
        if env is None:
            env = clean_env
        cmd = [sys.executable, "-m", "ncf"] + list(args)
        return subprocess.run(
            cmd,
            env=env,
            check=check,
            capture_output=capture_output,
            text=True,
        )

    return _run_ncf
