# Monitoring Dashboards and Alerts

This directory contains Python code for generating Grafana dashboards and alert
rules using the Grafana Foundation SDK. The generated files are stored in version
control and can be deployed to Grafana.

## Prerequisites

- Nix with flakes enabled
- Access to the devshell (`nix develop`)

## Directory Structure

```
monitoring/
  monitoring/              # Python package
    __init__.py
    __main__.py            # CLI entry point
    dashboards/
      __init__.py
      disk_space.py        # Disk space dashboard definition
    alerts/
      __init__.py
      disk_space.py        # Disk space alert rules definition
  generated/
    resources/
      dashboards/          # Generated dashboard manifests (grafanactl format)
      alerting/            # Generated alert rules (Grafana provisioning format)
  pyproject.toml           # Python project configuration
```

## Generating Dashboards and Alerts

From the repository root:

```bash
# Using nix run
nix run .#monitoring -- generate

# Or from within devshell
monitoring generate
```

This generates:

- Dashboard manifests in `monitoring/generated/resources/dashboards/`
- Alert rules in `monitoring/generated/resources/alerting/`

## Deploying Dashboards

1. Configure grafanactl with your Grafana instance:

```bash
grafanactl config set contexts.monitor.grafana.server https://monitor.lynx-lizard.ts.net
grafanactl config set contexts.monitor.grafana.org-id 1
grafanactl config set contexts.monitor.grafana.token YOUR_SERVICE_ACCOUNT_TOKEN
grafanactl config use-context monitor
```

2. Push the dashboards:

```bash
grafanactl resources push -p monitoring/generated/resources/dashboards/
```

## Deploying Alert Rules

Alert rules are generated in Grafana's file provisioning format. To deploy them:

1. Copy the generated YAML files to Grafana's provisioning directory:

```bash
cp monitoring/generated/resources/alerting/*.yaml /etc/grafana/provisioning/alerting/
```

2. Restart Grafana to load the new alert rules.

Alternatively, you can use the Grafana HTTP API to push alert rules programmatically.

## Current Alert Rules

The following alert rules are defined:

- **Disk Space Critical** - Fires when any filesystem has less than 5% free space
  (severity: critical, for: 5m)
- **Disk Space Warning** - Fires when any filesystem has less than 20% free space
  (severity: warning, for: 15m)

## Adding New Dashboards

1. Create a new file in `monitoring/monitoring/dashboards/` (e.g., `new_dashboard.py`)
2. Define the dashboard using the Grafana Foundation SDK builders
3. Export the builder function from `dashboards/__init__.py`
4. Add the dashboard generation to `__main__.py`
5. Run `monitoring generate` and commit both the Python code and generated JSON

## Adding New Alerts

1. Create a new file in `monitoring/monitoring/alerts/` (e.g., `new_alerts.py`)
2. Define the alert rules using the Grafana Foundation SDK alerting builders
3. Export the builder function from `alerts/__init__.py`
4. Add the alert generation to `__main__.py`
5. Run `monitoring generate` and commit both the Python code and generated YAML
