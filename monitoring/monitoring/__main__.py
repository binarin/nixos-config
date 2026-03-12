"""CLI entry point for monitoring dashboard and alert generation."""

import argparse
import json
import sys
from pathlib import Path
from typing import Any

import yaml

from grafana_foundation_sdk.cog.encoder import JSONEncoder
from grafana_foundation_sdk.models.resource import DashboardKind, Manifest, Metadata

from .alerts import build_disk_space_alerts
from .dashboards import build_disk_space_dashboard


def get_output_dir() -> Path:
    """Get the output directory for generated JSON files.

    By default, outputs to ./monitoring/generated/resources relative to
    the current working directory. This allows the command to be run from
    the repository root.
    """
    return Path.cwd() / "monitoring" / "generated" / "resources"


def generate_dashboards(output_dir: Path | None = None) -> None:
    """Generate all dashboard JSON files in grafanactl-compatible format.

    Generates dashboards wrapped in Kubernetes-style manifests that are
    compatible with 'grafanactl resources push'.
    """
    if output_dir is None:
        output_dir = get_output_dir()

    # Create dashboards subdirectory (grafanactl expects resources/dashboards/)
    dashboards_dir = output_dir / "dashboards"
    dashboards_dir.mkdir(parents=True, exist_ok=True)

    # Generate disk space dashboard
    dashboard_builder = build_disk_space_dashboard()
    dashboard = dashboard_builder.build()

    # Wrap in grafanactl-compatible manifest
    manifest = Manifest(
        api_version="dashboard.grafana.app/v1beta1",
        kind=DashboardKind,
        metadata=Metadata(name=dashboard.uid),
        spec=dashboard,
    )

    encoder = JSONEncoder(sort_keys=True, indent=2)
    json_content = encoder.encode(manifest)

    output_file = dashboards_dir / f"{dashboard.uid}.json"
    output_file.write_text(json_content)
    print(f"Generated: {output_file}")


def generate_alerts(output_dir: Path | None = None) -> None:
    """Generate alert rules in Grafana provisioning format.

    Generates alert rules in YAML format compatible with Grafana's file
    provisioning system. Files should be placed in the Grafana instance's
    provisioning/alerting directory.

    The format follows Grafana's alerting provisioning schema:
    https://grafana.com/docs/grafana/latest/alerting/set-up/provision-alerting-resources/file-provisioning/
    """
    if output_dir is None:
        output_dir = get_output_dir()

    # Create alerting subdirectory
    alerting_dir = output_dir / "alerting"
    alerting_dir.mkdir(parents=True, exist_ok=True)

    encoder = JSONEncoder(sort_keys=True, indent=2)

    # Build disk space alerts
    alert_rules = build_disk_space_alerts()

    # Convert rules to provisioning format
    rules_data: list[dict[str, Any]] = []
    for rule_builder in alert_rules:
        rule = rule_builder.build()
        # Use the encoder to convert to JSON, then parse back to dict for YAML
        rule_dict = json.loads(encoder.encode(rule))
        rules_data.append(rule_dict)

    # Build provisioning structure
    # Grafana file provisioning expects this format
    provisioning_data = {
        "apiVersion": 1,
        "groups": [
            {
                "orgId": 1,
                "name": "disk-space-alerts",
                "folder": "Disk Monitoring",
                "interval": "1m",
                "rules": rules_data,
            }
        ],
    }

    output_file = alerting_dir / "disk-space-alerts.yaml"
    with open(output_file, "w") as f:
        yaml.dump(provisioning_data, f, default_flow_style=False, sort_keys=False)
    print(f"Generated: {output_file}")


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Generate Grafana dashboards and alerts for NixOS monitoring"
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    # Generate command
    gen_parser = subparsers.add_parser(
        "generate", help="Generate dashboard and alert files"
    )
    gen_parser.add_argument(
        "--output-dir",
        type=Path,
        help="Output directory for generated files (default: monitoring/generated/resources)",
    )

    args = parser.parse_args()

    if args.command == "generate":
        generate_dashboards(args.output_dir)
        generate_alerts(args.output_dir)
        return 0

    return 1


if __name__ == "__main__":
    sys.exit(main())
