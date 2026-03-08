"""Disk space monitoring dashboard definition."""

from grafana_foundation_sdk.builders import (
    dashboard,
    prometheus,
    stat,
    table,
    timeseries,
    common as common_builder,
)
from grafana_foundation_sdk.models.common import (
    BigValueColorMode,
    BigValueGraphMode,
    GraphDrawStyle,
    GraphGradientMode,
    LegendDisplayMode,
    LegendPlacement,
    SortOrder,
    TooltipDisplayMode,
    VisibilityMode,
)
from grafana_foundation_sdk.models.dashboard import (
    DataSourceRef,
    DataTransformerConfig,
    DynamicConfigValue,
    ThresholdsMode,
    Threshold,
    VariableOption,
    VariableRefresh,
    VariableSort,
)
from grafana_foundation_sdk.models import prometheus as prom
from grafana_foundation_sdk.models import units

# VictoriaMetrics datasource UID configured in modules/machines/monitor.nix
DATASOURCE_UID = "P4169E866C3094E38"

# Filesystem types to exclude from monitoring (ephemeral/virtual filesystems)
FSTYPE_EXCLUDE = "tmpfs|overlay|nsfs|fuse|devtmpfs|squashfs"


def prometheus_query(query: str, legend: str = "") -> prometheus.Dataquery:
    """Create a Prometheus/VictoriaMetrics query."""
    q = prometheus.Dataquery().expr(query)
    if legend:
        q = q.legend_format(legend)
    return q


def table_query(query: str, ref_id: str) -> prometheus.Dataquery:
    """Create a table-format Prometheus query."""
    return (
        prometheus.Dataquery()
        .expr(query)
        .format(prom.PromQueryFormat.TABLE)
        .instant()
        .ref_id(ref_id)
    )


def default_stat() -> stat.Panel:
    """Create a stat panel with default settings."""
    return stat.Panel().height(4).span(6).datasource(DataSourceRef(uid=DATASOURCE_UID))


def default_timeseries() -> timeseries.Panel:
    """Create a time series panel with default settings."""
    return (
        timeseries.Panel()
        .height(8)
        .span(12)
        .line_width(1)
        .fill_opacity(10)
        .point_size(5)
        .show_points(VisibilityMode.AUTO)
        .draw_style(GraphDrawStyle.LINE)
        .gradient_mode(GraphGradientMode.NONE)
        .span_nulls(False)
        .datasource(DataSourceRef(uid=DATASOURCE_UID))
        .legend(
            common_builder.VizLegendOptions()
            .display_mode(LegendDisplayMode.TABLE)
            .placement(LegendPlacement.RIGHT)
            .show_legend(True)
        )
        .tooltip(
            common_builder.VizTooltipOptions()
            .mode(TooltipDisplayMode.MULTI)
            .sort(SortOrder.DESCENDING)
        )
    )


def disk_space_overview_table() -> table.Panel:
    """Table showing disk space for all filesystems across all hosts."""
    # Query for disk usage percentage
    usage_query = f"""
        100 - (
            node_filesystem_avail_bytes{{bhost=~"$host", mountpoint=~"$mountpoint", fstype!~"{FSTYPE_EXCLUDE}"}}
            /
            node_filesystem_size_bytes{{bhost=~"$host", mountpoint=~"$mountpoint", fstype!~"{FSTYPE_EXCLUDE}"}}
        ) * 100
    """.strip()

    # Query for available bytes
    avail_query = f"""
        node_filesystem_avail_bytes{{bhost=~"$host", mountpoint=~"$mountpoint", fstype!~"{FSTYPE_EXCLUDE}"}}
    """.strip()

    # Query for total size
    size_query = f"""
        node_filesystem_size_bytes{{bhost=~"$host", mountpoint=~"$mountpoint", fstype!~"{FSTYPE_EXCLUDE}"}}
    """.strip()

    return (
        table.Panel()
        .title("Disk Space Overview")
        .description(
            "All filesystems across all monitored hosts, sorted by usage percentage."
        )
        .height(10)
        .span(24)
        .datasource(DataSourceRef(uid=DATASOURCE_UID))
        .with_target(table_query(usage_query, "usage"))
        .with_target(table_query(avail_query, "avail"))
        .with_target(table_query(size_query, "size"))
        # Merge all queries into one table
        .with_transformation(DataTransformerConfig(id_val="merge", options={}))
        # Organize columns
        .with_transformation(
            DataTransformerConfig(
                id_val="organize",
                options={
                    "excludeByName": {
                        "Time": True,
                        "device": True,
                        "fstype": True,
                        "instance": True,
                        "job": True,
                    },
                    "indexByName": {
                        "bhost": 0,
                        "mountpoint": 1,
                        "Value #size": 2,
                        "Value #avail": 3,
                        "Value #usage": 4,
                    },
                    "renameByName": {
                        "bhost": "Host",
                        "mountpoint": "Mount Point",
                        "Value #size": "Total Size",
                        "Value #avail": "Available",
                        "Value #usage": "Used %",
                    },
                },
            )
        )
        # Sort by usage descending
        .with_transformation(
            DataTransformerConfig(
                id_val="sortBy",
                options={
                    "fields": {},
                    "sort": [{"field": "Used %", "desc": True}],
                },
            )
        )
        # Set units and thresholds for columns
        .override_by_name(
            "Total Size",
            [DynamicConfigValue(id_val="unit", value=units.BytesIEC)],
        )
        .override_by_name(
            "Available",
            [DynamicConfigValue(id_val="unit", value=units.BytesIEC)],
        )
        .override_by_name(
            "Used %",
            [
                DynamicConfigValue(id_val="unit", value=units.Percent),
                DynamicConfigValue(id_val="decimals", value=1),
                DynamicConfigValue(
                    id_val="thresholds",
                    value={
                        "mode": "absolute",
                        "steps": [
                            {"color": "green", "value": None},
                            {"color": "yellow", "value": 80},
                            {"color": "red", "value": 95},
                        ],
                    },
                ),
                DynamicConfigValue(
                    id_val="custom.cellOptions", value={"type": "color-background"}
                ),
            ],
        )
    )


def disk_usage_by_host_timeseries() -> timeseries.Panel:
    """Time series showing disk usage percentage for root filesystem by host."""
    query = f"""
        100 - (
            node_filesystem_avail_bytes{{bhost=~"$host", mountpoint="/", fstype!~"{FSTYPE_EXCLUDE}"}}
            /
            node_filesystem_size_bytes{{bhost=~"$host", mountpoint="/", fstype!~"{FSTYPE_EXCLUDE}"}}
        ) * 100
    """.strip()

    return (
        default_timeseries()
        .title("Root Filesystem Usage by Host")
        .description(
            "Percentage of disk space used on the root filesystem (/) over time."
        )
        .span(12)
        .with_target(prometheus_query(query, "{{ bhost }}"))
        .unit(units.Percent)
        .min(0)
        .max(100)
        .thresholds(
            dashboard.ThresholdsConfig()
            .mode(ThresholdsMode.ABSOLUTE)
            .steps(
                [
                    Threshold(color="green"),
                    Threshold(value=80, color="yellow"),
                    Threshold(value=95, color="red"),
                ]
            )
        )
    )


def available_space_timeseries() -> timeseries.Panel:
    """Time series showing available disk space over time."""
    query = f"""
        node_filesystem_avail_bytes{{bhost=~"$host", mountpoint=~"$mountpoint", fstype!~"{FSTYPE_EXCLUDE}"}}
    """.strip()

    return (
        default_timeseries()
        .title("Available Disk Space")
        .description("Available disk space over time for selected filesystems.")
        .span(12)
        .with_target(prometheus_query(query, "{{ bhost }} {{ mountpoint }}"))
        .unit(units.BytesIEC)
        .min(0)
    )


def low_space_stat() -> stat.Panel:
    """Stat panel highlighting filesystems with less than 20% free space."""
    # Count of filesystems below 20% free
    query = f"""
        count(
            (
                node_filesystem_avail_bytes{{fstype!~"{FSTYPE_EXCLUDE}"}}
                /
                node_filesystem_size_bytes{{fstype!~"{FSTYPE_EXCLUDE}"}}
            ) < 0.2
        ) or vector(0)
    """.strip()

    return (
        default_stat()
        .title("Low Space Filesystems")
        .description("Number of filesystems with less than 20% free space.")
        .span(6)
        .with_target(prometheus_query(query, "Count"))
        .decimals(0)
        .color_mode(BigValueColorMode.VALUE)
        .graph_mode(BigValueGraphMode.NONE)
        .thresholds(
            dashboard.ThresholdsConfig()
            .mode(ThresholdsMode.ABSOLUTE)
            .steps(
                [
                    Threshold(color="green"),
                    Threshold(value=1, color="yellow"),
                    Threshold(value=3, color="red"),
                ]
            )
        )
    )


def critical_space_stat() -> stat.Panel:
    """Stat panel highlighting filesystems with less than 5% free space."""
    query = f"""
        count(
            (
                node_filesystem_avail_bytes{{fstype!~"{FSTYPE_EXCLUDE}"}}
                /
                node_filesystem_size_bytes{{fstype!~"{FSTYPE_EXCLUDE}"}}
            ) < 0.05
        ) or vector(0)
    """.strip()

    return (
        default_stat()
        .title("Critical Space Filesystems")
        .description("Number of filesystems with less than 5% free space.")
        .span(6)
        .with_target(prometheus_query(query, "Count"))
        .decimals(0)
        .color_mode(BigValueColorMode.VALUE)
        .graph_mode(BigValueGraphMode.NONE)
        .thresholds(
            dashboard.ThresholdsConfig()
            .mode(ThresholdsMode.ABSOLUTE)
            .steps(
                [
                    Threshold(color="green"),
                    Threshold(value=1, color="red"),
                ]
            )
        )
    )


def min_free_space_stat() -> stat.Panel:
    """Stat panel showing the minimum free space percentage across all filesystems."""
    query = f"""
        min(
            node_filesystem_avail_bytes{{fstype!~"{FSTYPE_EXCLUDE}"}}
            /
            node_filesystem_size_bytes{{fstype!~"{FSTYPE_EXCLUDE}"}}
        ) * 100
    """.strip()

    return (
        default_stat()
        .title("Minimum Free Space")
        .description("Lowest free space percentage across all monitored filesystems.")
        .span(6)
        .with_target(prometheus_query(query, "Min Free"))
        .unit(units.Percent)
        .decimals(1)
        .color_mode(BigValueColorMode.VALUE)
        .graph_mode(BigValueGraphMode.AREA)
        .thresholds(
            dashboard.ThresholdsConfig()
            .mode(ThresholdsMode.ABSOLUTE)
            .steps(
                [
                    Threshold(color="red"),
                    Threshold(value=5, color="yellow"),
                    Threshold(value=20, color="green"),
                ]
            )
        )
    )


def total_hosts_stat() -> stat.Panel:
    """Stat panel showing the total number of monitored hosts."""
    query = "count(count by (bhost) (node_filesystem_size_bytes))"

    return (
        default_stat()
        .title("Monitored Hosts")
        .description("Total number of hosts being monitored.")
        .span(6)
        .with_target(prometheus_query(query, "Hosts"))
        .decimals(0)
        .color_mode(BigValueColorMode.NONE)
        .graph_mode(BigValueGraphMode.NONE)
    )


def build_disk_space_dashboard() -> dashboard.Dashboard:
    """Build the complete disk space monitoring dashboard."""
    return (
        dashboard.Dashboard("Disk Space Monitoring")
        .uid("disk-space-monitoring")
        .tags(["generated", "infrastructure", "disk", "storage"])
        .editable()
        .refresh("1m")
        .time("now-1h", "now")
        .timezone("browser")
        .timepicker(
            dashboard.TimePicker().refresh_intervals(
                ["30s", "1m", "5m", "15m", "30m", "1h", "2h", "1d"]
            )
        )
        # Host variable
        .with_variable(
            dashboard.QueryVariable("host")
            .label("Host")
            .query("label_values(node_filesystem_size_bytes, bhost)")
            .datasource(DataSourceRef(uid=DATASOURCE_UID))
            .current(VariableOption(selected=True, text="All", value="$__all"))
            .refresh(VariableRefresh.ON_TIME_RANGE_CHANGED)
            .sort(VariableSort.ALPHABETICAL_ASC)
            .multi(True)
            .include_all(True)
        )
        # Mountpoint variable
        .with_variable(
            dashboard.QueryVariable("mountpoint")
            .label("Mount Point")
            .query(
                f'label_values(node_filesystem_size_bytes{{fstype!~"{FSTYPE_EXCLUDE}"}}, mountpoint)'
            )
            .datasource(DataSourceRef(uid=DATASOURCE_UID))
            .current(VariableOption(selected=True, text="All", value="$__all"))
            .refresh(VariableRefresh.ON_TIME_RANGE_CHANGED)
            .sort(VariableSort.ALPHABETICAL_ASC)
            .multi(True)
            .include_all(True)
        )
        # Summary row
        .with_row(dashboard.Row("Summary"))
        .with_panel(total_hosts_stat())
        .with_panel(min_free_space_stat())
        .with_panel(low_space_stat())
        .with_panel(critical_space_stat())
        # Overview row
        .with_row(dashboard.Row("Overview"))
        .with_panel(disk_space_overview_table())
        # Trends row
        .with_row(dashboard.Row("Trends"))
        .with_panel(disk_usage_by_host_timeseries())
        .with_panel(available_space_timeseries())
    )
