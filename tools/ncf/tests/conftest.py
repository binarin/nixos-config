"""Pytest configuration and fixtures for ncf tests."""

import os
import subprocess
import sys

import pytest


@pytest.fixture
def run_ncf():
    """Fixture to run ncf commands.

    Environment isolation is handled by nix (--ignore-env), not Python.
    """

    def _run_ncf(*args, check=True, capture_output=True, env=None):
        cmd = [sys.executable, "-m", "ncf"] + list(args)
        # Merge extra env vars with current environment
        run_env = os.environ.copy()
        if env:
            run_env.update(env)
        return subprocess.run(
            cmd,
            check=check,
            capture_output=capture_output,
            text=True,
            env=run_env,
        )

    return _run_ncf
