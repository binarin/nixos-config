"""Grafana alert rules definitions."""

from .disk_space import build_disk_space_alerts

__all__ = ["build_disk_space_alerts"]
