"""Tests for external tool dependencies.

These tests verify that all external tools required by ncf are available
in PATH when running in the nix develop shell.

Run with: nix develop --ignore-env -c pytest tools/ncf/tests/test_external_deps.py
"""

import shutil

import pytest


def get_all_tools():
    """Import all modules to trigger tool registrations and return tools."""
    # Import all command modules to ensure their tool registrations are executed
    from ncf.commands import (  # noqa: F401
        add_machine,
        build,
        ci,
        eval,
        generate,
        init_machine,
        ipam_cmd,
        iso,
        list_machines,
        provision_lxc,
        set_secret,
        verify,
    )
    from ncf import nix  # noqa: F401
    from ncf.external import get_required_tools, get_optional_tools

    return get_required_tools(), get_optional_tools()


# Get tools at module load time
REQUIRED_TOOLS, OPTIONAL_TOOLS = get_all_tools()


class TestExternalDeps:
    """Test that external dependencies are available."""

    @pytest.mark.parametrize(
        "tool",
        REQUIRED_TOOLS,
        ids=lambda t: t.name,
    )
    def test_required_tool_available(self, tool):
        """Test that a required tool is available in PATH."""
        path = shutil.which(tool.name)
        purposes = ", ".join(r.purpose for r in tool.registrations)
        assert path is not None, f"{tool.name} not found in PATH (used for: {purposes})"

    @pytest.mark.parametrize(
        "tool",
        OPTIONAL_TOOLS,
        ids=lambda t: t.name,
    )
    def test_optional_tool_available(self, tool):
        """Test that an optional tool is available in PATH."""
        path = shutil.which(tool.name)
        purposes = ", ".join(r.purpose for r in tool.registrations)
        assert path is not None, f"{tool.name} not found in PATH (used for: {purposes})"

    def test_all_tools_summary(self):
        """Summary test that lists all missing tools."""
        all_tools = REQUIRED_TOOLS + OPTIONAL_TOOLS
        missing = []
        for tool in all_tools:
            if shutil.which(tool.name) is None:
                purposes = ", ".join(r.purpose for r in tool.registrations)
                req = "required" if tool.required else "optional"
                missing.append(f"  - {tool.name} ({req}): {purposes}")

        if missing:
            missing_list = "\n".join(missing)
            pytest.fail(f"Missing external tools:\n{missing_list}")
