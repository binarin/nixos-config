# NCF: Route diagnostic output to stderr

## Problem

`ncf ci matrix` outputs a JSON array to stdout, but warnings (e.g., "Could not
get CI config for iso-installer") are also printed to stdout via Rich's
`Console()`, which defaults to stdout. This pollutes machine-readable output.

The root cause: every file in ncf creates its own `console = Console()` which
writes to stdout. There is no separation between diagnostic and data output
streams.

## Solution

Create a shared stderr console and use it everywhere.

### New file: `tools/ncf/ncf/output.py`

```python
from rich.console import Console

console = Console(stderr=True)
```

### Migration

All 22 files that currently do:

```python
from rich.console import Console
console = Console()
```

Change to:

```python
from ncf.output import console
```

(Using relative imports where appropriate: `from ..output import console` or
`from .output import console` depending on module depth.)

### Files to update

Top-level modules:
- `ncf/cli.py`
- `ncf/external.py`
- `ncf/nix.py`
- `ncf/secrets_inject.py`

Command modules (`ncf/commands/`):
- `add_machine.py`
- `aws_env.py`
- `build.py`
- `ci.py`
- `deploy.py`
- `eval.py`
- `generate.py`
- `init_machine.py`
- `ipam_cmd.py`
- `iso.py`
- `iso_installer.py`
- `list_machines.py`
- `provision_lxc.py`
- `provision_vm.py`
- `provision_vm_anywhere.py`
- `set_secret.py`
- `tailscale.py`
- `verify.py`

### Special case: `list_machines.py`

Line 53 uses `console.print(json.dumps(output, indent=2))` for JSON output.
This must change to bare `print(json.dumps(output, indent=2))` so JSON goes to
stdout while diagnostics go to stderr.

### Intentional stderr moves

Rich Table output in `list_machines.py` (line 111) and `ci.py` (`run_external_deps`,
line 387) will move from stdout to stderr. This is correct — tables are
human-readable displays, not machine-parseable output.

### What stays the same

- All existing `print()` calls remain on stdout (these are data output)
- Rich markup/colors still work on stderr in terminals
- No behavioral change for interactive use — users see the same output
- `Console(stderr=True)` is a standard Rich feature

## Validation

1. `ncf ci matrix 2>/dev/null` should produce clean JSON
2. `ncf ci matrix 2>&1` should show warnings alongside JSON (current behavior)
3. `ncf list --json 2>/dev/null` should produce clean JSON
4. `ncf eval nixos` and other interactive commands should look identical
