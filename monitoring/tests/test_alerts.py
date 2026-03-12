"""Tests for Grafana alert rule generation.

These tests verify that alert rule definitions build correctly and produce
valid, well-formed data that can be used with Grafana's provisioning system.
"""

import json

from grafana_foundation_sdk.cog.encoder import JSONEncoder

from monitoring.alerts import build_disk_space_alerts


class TestDiskSpaceAlerts:
    """Tests for the disk space monitoring alert rules."""

    def test_builds_successfully(self):
        """Test that the alert rules build without errors."""
        rules = build_disk_space_alerts()
        assert rules is not None
        assert len(rules) == 2  # Critical and warning

    def test_critical_alert_properties(self):
        """Test that the critical alert has correct properties."""
        rules = build_disk_space_alerts()
        critical_rule = rules[0].build()
        assert critical_rule.title == "Disk Space Critical - Less than 5% Free"
        assert critical_rule.uid == "disk-space-critical"
        assert critical_rule.labels is not None
        assert critical_rule.labels.get("severity") == "critical"
        assert critical_rule.for_val == "5m"

    def test_warning_alert_properties(self):
        """Test that the warning alert has correct properties."""
        rules = build_disk_space_alerts()
        warning_rule = rules[1].build()
        assert warning_rule.title == "Disk Space Warning - Less than 20% Free"
        assert warning_rule.uid == "disk-space-warning"
        assert warning_rule.labels is not None
        assert warning_rule.labels.get("severity") == "warning"
        assert warning_rule.for_val == "15m"

    def test_json_serializable(self):
        """Test that alert rules can be serialized to valid JSON."""
        rules = build_disk_space_alerts()
        encoder = JSONEncoder(sort_keys=True, indent=2)
        for rule_builder in rules:
            rule = rule_builder.build()
            json_str = encoder.encode(rule)
            # Verify it's valid JSON
            parsed = json.loads(json_str)
            assert "title" in parsed
            assert "condition" in parsed
            assert "data" in parsed

    def test_has_required_queries(self):
        """Test that alert rules have the required query structure."""
        rules = build_disk_space_alerts()
        for rule_builder in rules:
            rule = rule_builder.build()
            assert rule.data is not None
            assert len(rule.data) == 3  # Prometheus query, reduce, threshold
            # Check query types
            ref_ids = [q.ref_id for q in rule.data]
            assert "A" in ref_ids  # Prometheus query
            assert "B" in ref_ids  # Reduce expression
            assert "C" in ref_ids  # Threshold expression

    def test_condition_references_threshold(self):
        """Test that the condition references the threshold expression."""
        rules = build_disk_space_alerts()
        for rule_builder in rules:
            rule = rule_builder.build()
            # The condition should reference the threshold expression (C)
            assert rule.condition == "C"
