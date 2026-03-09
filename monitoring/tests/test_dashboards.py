"""Tests for Grafana dashboard generation.

These tests verify that dashboard definitions build correctly and produce
valid, well-formed JSON that can be used with grafanactl.
"""

import json

from grafana_foundation_sdk.cog.encoder import JSONEncoder

from monitoring.dashboards import build_disk_space_dashboard


class TestDiskSpaceDashboard:
    """Tests for the disk space monitoring dashboard."""

    def test_builds_successfully(self):
        """Test that the dashboard builds without errors."""
        builder = build_disk_space_dashboard()
        dashboard = builder.build()
        assert dashboard is not None

    def test_has_required_properties(self):
        """Test that the dashboard has required properties set correctly."""
        dashboard = build_disk_space_dashboard().build()
        assert dashboard.uid == "disk-space-monitoring"
        assert dashboard.title == "Disk Space Monitoring"
        assert "generated" in dashboard.tags
        assert "infrastructure" in dashboard.tags

    def test_json_serializable(self):
        """Test that the dashboard can be serialized to valid JSON."""
        dashboard = build_disk_space_dashboard().build()
        encoder = JSONEncoder(sort_keys=True, indent=2)
        json_str = encoder.encode(dashboard)
        # Verify it's valid JSON
        parsed = json.loads(json_str)
        assert "uid" in parsed
        assert parsed["uid"] == "disk-space-monitoring"

    def test_has_variables(self):
        """Test that the dashboard has the expected template variables."""
        dashboard = build_disk_space_dashboard().build()
        # Templating contains list of variables
        assert dashboard.templating is not None
        assert dashboard.templating.list_val is not None
        var_names = [v.name for v in dashboard.templating.list_val]
        assert "host" in var_names
        assert "mountpoint" in var_names

    def test_has_panels(self):
        """Test that the dashboard has panels defined."""
        dashboard = build_disk_space_dashboard().build()
        # Panels are stored in the panels list
        assert dashboard.panels is not None
        assert len(dashboard.panels) > 0
