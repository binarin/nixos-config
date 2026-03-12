"""Disk space monitoring alert rules definition."""

from grafana_foundation_sdk.builders import alerting, prometheus, expr
from grafana_foundation_sdk.models.alerting import RelativeTimeRange
from grafana_foundation_sdk.models.dashboard import DataSourceRef

# VictoriaMetrics datasource UID configured in modules/machines/monitor.nix
DATASOURCE_UID = "P4169E866C3094E38"

# Filesystem types to exclude from monitoring (ephemeral/virtual filesystems)
FSTYPE_EXCLUDE = "tmpfs|overlay|nsfs|fuse|devtmpfs|squashfs"

# Expression datasource for reduce/threshold operations
EXPR_DATASOURCE = DataSourceRef(type_val="__expr__", uid="__expr__")


def _free_space_query() -> str:
    """Return the PromQL query for free disk space percentage."""
    return (
        f'(node_filesystem_avail_bytes{{fstype!~"{FSTYPE_EXCLUDE}"}} / '
        f'node_filesystem_size_bytes{{fstype!~"{FSTYPE_EXCLUDE}"}}) * 100'
    )


def _build_disk_space_alert(
    uid: str,
    title: str,
    threshold_pct: float,
    severity: str,
    for_duration: str = "5m",
) -> alerting.Rule:
    """Build a disk space alert rule with the given threshold.

    Args:
        uid: Unique identifier for the rule (max 40 chars, alphanumeric with - and _)
        title: Human-readable title for the alert
        threshold_pct: Percentage threshold below which to alert (e.g., 5.0 for 5%)
        severity: Severity label (critical, warning, info)
        for_duration: How long the condition must be true before firing

    Returns:
        An alerting.Rule builder configured for the threshold
    """
    return (
        alerting.Rule(title)
        .uid(uid)
        .folder_uid("disk-monitoring")
        .rule_group("disk-space-alerts")
        .condition("C")
        .for_val(for_duration)
        .exec_err_state("Error")
        .no_data_state("OK")
        .annotations(
            {
                "summary": (
                    f"{{{{ $labels.bhost }}}}:{{{{ $labels.mountpoint }}}} "
                    f"has less than {threshold_pct:.0f}% free space"
                ),
                "description": (
                    "Filesystem {{ $labels.mountpoint }} on {{ $labels.bhost }} "
                    'has only {{ printf "%.1f" $values.B.Value }}% free space remaining.'
                ),
            }
        )
        .labels(
            {
                "severity": severity,
            }
        )
        # Query A: Prometheus query for free space percentage
        .with_query(
            alerting.Query("A")
            .datasource_uid(DATASOURCE_UID)
            .query_type("")
            .model(
                prometheus.Dataquery().expr(_free_space_query()).ref_id("A").instant()
            )
            .relative_time_range(RelativeTimeRange(from_val=600, to=0))
        )
        # Query B: Reduce to last value
        .with_query(
            alerting.Query("B")
            .datasource_uid("__expr__")
            .query_type("")
            .model(
                expr.TypeReduce()
                .ref_id("B")
                .expression("A")
                .reducer("last")
                .datasource(EXPR_DATASOURCE)
            )
            .relative_time_range(RelativeTimeRange(from_val=600, to=0))
        )
        # Query C: Threshold condition
        .with_query(
            alerting.Query("C")
            .datasource_uid("__expr__")
            .query_type("")
            .model(
                expr.TypeThreshold()
                .ref_id("C")
                .expression("B")
                .conditions(
                    [
                        expr.ExprTypeThresholdConditions().evaluator(
                            expr.ExprTypeThresholdConditionsEvaluator()
                            .params([threshold_pct])
                            .type("lt")
                        )
                    ]
                )
                .datasource(EXPR_DATASOURCE)
            )
            .relative_time_range(RelativeTimeRange(from_val=600, to=0))
        )
    )


def build_disk_space_alerts() -> list[alerting.Rule]:
    """Build all disk space alert rules.

    Returns:
        List of alert rule builders
    """
    return [
        # Critical: Less than 5% free space
        _build_disk_space_alert(
            uid="disk-space-critical",
            title="Disk Space Critical - Less than 5% Free",
            threshold_pct=5.0,
            severity="critical",
            for_duration="5m",
        ),
        # Warning: Less than 20% free space
        _build_disk_space_alert(
            uid="disk-space-warning",
            title="Disk Space Warning - Less than 20% Free",
            threshold_pct=20.0,
            severity="warning",
            for_duration="15m",
        ),
    ]
