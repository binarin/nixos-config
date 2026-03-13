"""Shared console for diagnostic output.

All ncf diagnostic/warning output goes to stderr via this console.
Machine-readable output (JSON, paths) should use bare print() to go to stdout.
"""

from rich.console import Console

console = Console(stderr=True)
