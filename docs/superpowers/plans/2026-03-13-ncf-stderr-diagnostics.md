# NCF stderr diagnostics Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Route all ncf diagnostic/warning output to stderr, keeping stdout clean for machine-readable data.

**Architecture:** Create a shared `output.py` module with `Console(stderr=True)`, replace all per-file `Console()` instances with imports from this module, fix one case where `console.print` is used for data output.

**Tech Stack:** Python, Rich (Console), Typer CLI

**Spec:** `docs/superpowers/specs/2026-03-13-ncf-stderr-diagnostics-design.md`

---

## Chunk 1: Implementation

### Task 1: Create shared output module

**Files:**
- Create: `tools/ncf/ncf/output.py`

- [ ] **Step 1: Create `tools/ncf/ncf/output.py`**

```python
"""Shared console for diagnostic output.

All ncf diagnostic/warning output goes to stderr via this console.
Machine-readable output (JSON, paths) should use bare print() to go to stdout.
"""

from rich.console import Console

console = Console(stderr=True)
```

- [ ] **Step 2: `git add` the new file**

```bash
git add tools/ncf/ncf/output.py
```

### Task 2: Migrate top-level modules

**Files:**
- Modify: `tools/ncf/ncf/cli.py` — change import
- Modify: `tools/ncf/ncf/external.py` — change import
- Modify: `tools/ncf/ncf/nix.py` — change import
- Modify: `tools/ncf/ncf/secrets_inject.py` — change import

For each file, replace:
```python
from rich.console import Console
# ... (other imports between)
console = Console()
```

With:
```python
from .output import console
```

Remove the `from rich.console import Console` line ONLY if `Console` is not used elsewhere in the file. If `Console` is used elsewhere (e.g., passed as a type or constructed differently), keep the import but still remove the `console = Console()` line.

- [ ] **Step 1: Update `tools/ncf/ncf/cli.py`**
- [ ] **Step 2: Update `tools/ncf/ncf/external.py`**
- [ ] **Step 3: Update `tools/ncf/ncf/nix.py`**
- [ ] **Step 4: Update `tools/ncf/ncf/secrets_inject.py`**
- [ ] **Step 5: Commit**

```bash
git add tools/ncf/ncf/output.py tools/ncf/ncf/cli.py tools/ncf/ncf/external.py tools/ncf/ncf/nix.py tools/ncf/ncf/secrets_inject.py
git commit -m "feat(ncf): create shared stderr console and migrate top-level modules"
```

### Task 3: Migrate command modules (batch 1: a-d)

**Files:**
- Modify: `tools/ncf/ncf/commands/add_machine.py`
- Modify: `tools/ncf/ncf/commands/aws_env.py`
- Modify: `tools/ncf/ncf/commands/build.py`
- Modify: `tools/ncf/ncf/commands/ci.py`
- Modify: `tools/ncf/ncf/commands/deploy.py`

Same pattern — replace:
```python
from rich.console import Console
# ...
console = Console()
```

With:
```python
from ..output import console
```

Note: `from ..output` (double dot) because these are in the `commands/` subdirectory.

- [ ] **Step 1: Update `add_machine.py`**
- [ ] **Step 2: Update `aws_env.py`**
- [ ] **Step 3: Update `build.py`**
- [ ] **Step 4: Update `ci.py`**
- [ ] **Step 5: Update `deploy.py`**
- [ ] **Step 6: Commit**

```bash
git add tools/ncf/ncf/commands/{add_machine,aws_env,build,ci,deploy}.py
git commit -m "feat(ncf): migrate command modules a-d to shared stderr console"
```

### Task 4: Migrate command modules (batch 2: e-l)

**Files:**
- Modify: `tools/ncf/ncf/commands/eval.py`
- Modify: `tools/ncf/ncf/commands/generate.py`
- Modify: `tools/ncf/ncf/commands/init_machine.py`
- Modify: `tools/ncf/ncf/commands/ipam_cmd.py`
- Modify: `tools/ncf/ncf/commands/iso.py`
- Modify: `tools/ncf/ncf/commands/iso_installer.py`
- Modify: `tools/ncf/ncf/commands/list_machines.py` — **special case, see below**

For `list_machines.py`, in addition to the import change, also change line 53:
```python
# Before:
        console.print(json.dumps(output, indent=2))
# After:
        print(json.dumps(output, indent=2))
```

This ensures JSON data output goes to stdout, not stderr.

- [ ] **Step 1: Update `eval.py`**
- [ ] **Step 2: Update `generate.py`**
- [ ] **Step 3: Update `init_machine.py`**
- [ ] **Step 4: Update `ipam_cmd.py`**
- [ ] **Step 5: Update `iso.py`**
- [ ] **Step 6: Update `iso_installer.py`**
- [ ] **Step 7: Update `list_machines.py`** (import change + `console.print(json.dumps(...))` → `print(json.dumps(...))`)
- [ ] **Step 8: Commit**

```bash
git add tools/ncf/ncf/commands/{eval,generate,init_machine,ipam_cmd,iso,iso_installer,list_machines}.py
git commit -m "feat(ncf): migrate command modules e-l to shared stderr console"
```

### Task 5: Migrate command modules (batch 3: p-v)

**Files:**
- Modify: `tools/ncf/ncf/commands/provision_lxc.py`
- Modify: `tools/ncf/ncf/commands/provision_vm.py`
- Modify: `tools/ncf/ncf/commands/provision_vm_anywhere.py`
- Modify: `tools/ncf/ncf/commands/set_secret.py`
- Modify: `tools/ncf/ncf/commands/tailscale.py`
- Modify: `tools/ncf/ncf/commands/verify.py`

Same import pattern as Tasks 3-4.

- [ ] **Step 1: Update `provision_lxc.py`**
- [ ] **Step 2: Update `provision_vm.py`**
- [ ] **Step 3: Update `provision_vm_anywhere.py`**
- [ ] **Step 4: Update `set_secret.py`**
- [ ] **Step 5: Update `tailscale.py`**
- [ ] **Step 6: Update `verify.py`**
- [ ] **Step 7: Commit**

```bash
git add tools/ncf/ncf/commands/{provision_lxc,provision_vm,provision_vm_anywhere,set_secret,tailscale,verify}.py
git commit -m "feat(ncf): migrate command modules p-v to shared stderr console"
```

### Task 6: Validate and format

- [ ] **Step 1: Run `ncf eval nixos`** to check the tool still works (timeout 5 min)
- [ ] **Step 2: Run `nix fmt`** and **`just lint`** to fix formatting
- [ ] **Step 3: Amend last commit if formatting changed**

```bash
nix fmt
just lint
git add -u tools/ncf/
git commit --amend --no-edit
```

- [ ] **Step 4: Run `ncf ci matrix 2>/dev/null`** and verify output is clean JSON with no warnings
- [ ] **Step 5: Run `ncf ci matrix`** and verify warnings appear on stderr (visible in terminal but not in redirected stdout)
