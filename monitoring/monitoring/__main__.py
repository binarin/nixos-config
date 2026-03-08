"""CLI entry point for monitoring dashboard generation."""

import argparse
import sys
from pathlib import Path

from grafana_foundation_sdk.cog.encoder import JSONEncoder
from grafana_foundation_sdk.models.resource import DashboardKind, Manifest, Metadata

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


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Generate Grafana dashboards for NixOS monitoring"
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    # Generate command
    gen_parser = subparsers.add_parser("generate", help="Generate dashboard JSON files")
    gen_parser.add_argument(
        "--output-dir",
        type=Path,
        help="Output directory for generated files (default: monitoring/generated/resources)",
    )

    args = parser.parse_args()

    if args.command == "generate":
        generate_dashboards(args.output_dir)
        return 0

    return 1


if __name__ == "__main__":
    sys.exit(main())
