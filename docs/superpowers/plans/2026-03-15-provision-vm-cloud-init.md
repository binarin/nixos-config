# Provision VM Cloud-Init Simplification — Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Simplify `provision-vm-anywhere` to only create VMs with cloud-init IP assignment, removing the nixos-anywhere installation steps that are now handled by `clan install`.

**Architecture:** Remove nixos-anywhere integration from provision-vm-anywhere, add cloud-init drive attachment, enable cloud-init in installer ISO, and add force-rebuild-iso flag.

**Tech Stack:** Python (typer CLI), Nix (NixOS modules)

**Spec:** `docs/superpowers/specs/2026-03-15-provision-vm-cloud-init-design.md`

---

## Task 1: Enable cloud-init in installer ISO

**Files:**
- Modify: `modules/machines/iso.nix:40-131` (inside `config` block)

- [x] **Step 1: Add cloud-init to iso.nix**

In `modules/machines/iso.nix`, add inside the `config` block (after the `services.tailscale.enable` line):

```nix
services.cloud-init.enable = true;
```

- [x] **Step 2: Verify nix evaluation**

Run: `nix eval .#nixosConfigurations.iso.config.services.cloud-init.enable`
Expected: `true`

- [x] **Step 3: Commit**

```bash
git add modules/machines/iso.nix
git commit -m "feat(iso): enable cloud-init in installer ISO"
```

---

## Task 2: Fix cloud-init storage default in qemu-guest.nix

**Files:**
- Modify: `modules/qemu-guest.nix:249-253`

- [x] **Step 1: Change default storage**

In `modules/qemu-guest.nix`, change the `cloudInit.storage` default from `"local"` to `"local-zfs"`:

```nix
storage = lib.mkOption {
  type = lib.types.str;
  default = "local-zfs";
  description = "Storage for cloud-init drive.";
};
```

- [x] **Step 2: Verify nix evaluation**

Pick any qemu-guest machine (e.g., `llm-runner`) and check:
Run: `nix eval .#nixosConfigurations.llm-runner.config.nixos-config.qemu-guest.proxmox.cloudInit.storage`
Expected: `"local-zfs"`

- [x] **Step 3: Commit**

```bash
git add modules/qemu-guest.nix
git commit -m "fix(qemu-guest): default cloud-init storage to local-zfs"
```

---

## Task 3: Always pass --ipconfig0 in build_qm_create_command

**Files:**
- Modify: `tools/ncf/ncf/commands/provision_vm.py:73-158` (`build_qm_create_command`)

- [x] **Step 1: Move ipconfig0 out of cloud_init_snippet guard**

In `build_qm_create_command`, the `--ipconfig0` is currently inside `if cloud_init_snippet:`. Move it to always be set. Replace lines 147-156:

```python
# Cloud-init snippet (optional)
if cloud_init_snippet:
    cmd.extend(["--cicustom", f"user={cloud_init_snippet}"])

# Static IP configuration (always set for cloud-init)
ip_addr = network_config.get("address")
prefix = network_config.get("prefix")
gateway = network_config.get("gateway")
if ip_addr and prefix and gateway:
    cmd.extend(["--ipconfig0", f"ip={ip_addr}/{prefix},gw={gateway}"])
```

- [x] **Step 2: Commit**

```bash
git add tools/ncf/ncf/commands/provision_vm.py
git commit -m "feat(provision): always pass --ipconfig0 for cloud-init IP"
```

---

## Task 4: Add force parameter to ensure_iso_on_proxmox

**Files:**
- Modify: `tools/ncf/ncf/commands/iso_installer.py:145-195` (`ensure_iso_on_proxmox`)

- [x] **Step 1: Add force parameter**

Add `force: bool = False` parameter to `ensure_iso_on_proxmox`. When `force=True`, rebuild and re-upload even if ISO exists:

```python
def ensure_iso_on_proxmox(
    proxmox_host: str,
    storage: str = "local",
    build_if_missing: bool = True,
    force: bool = False,
    verbosity: int = 1,
    dry_run: bool = False,
) -> str:
```

Replace the early-return check (lines 177-179):

```python
# Check if already on Proxmox
if not force and client.iso_exists(storage, ISO_FILENAME):
    console.print(f"  [green]✓[/green] ISO already present: {ISO_FILENAME}")
    return iso_ref
```

When force is true, also force rebuild of the local ISO:

```python
# Build if needed
if force or not local_iso.exists():
    if build_if_missing or force:
        console.print("  Building installer ISO...")
        run_build(output=local_iso, verbosity=verbosity)
    else:
        raise RuntimeError(f"ISO not found at {local_iso}")
```

- [x] **Step 2: Commit**

```bash
git add tools/ncf/ncf/commands/iso_installer.py
git commit -m "feat(iso): add force parameter to ensure_iso_on_proxmox"
```

---

## Task 5: Simplify provision_vm_anywhere.py

**Files:**
- Modify: `tools/ncf/ncf/commands/provision_vm_anywhere.py`

- [x] **Step 1: Remove run_nixos_anywhere function and unused imports**

Remove these imports (lines 12-14):
```python
import shutil
import tempfile
```

Remove this import (line 24):
```python
from ..secrets_inject import gather_secrets_for_machine, decrypt_secrets_to_tempdir
```

Remove the entire `run_nixos_anywhere` function (lines 67-118).

- [x] **Step 2: Simplify run() signature**

Change the `run()` function signature to:

```python
def run(
    machine: str,
    proxmox_host: str,
    bridge: str = "vmbr0",
    ssh_timeout: int = 300,
    force_rebuild_iso: bool = False,
    dry_run: bool = False,
) -> None:
```

Update the docstring to reflect the simplified workflow.

- [x] **Step 3: Remove sops preflight**

Remove lines 155-174 (the entire preflight section that gathers and tests secrets).

- [x] **Step 4: Add cloud-init drive attachment**

After the disk configuration step (after the disks loop, around line 357), add a new step to attach the cloud-init drive:

```python
# Step: Attach cloud-init drive
console.print("\n[bold]Step 5c:[/bold] Attaching cloud-init drive")
cloud_init_config = vm_config.get("cloudInit", {})
ci_storage = cloud_init_config.get("storage", "local-zfs")

if dry_run:
    console.print(f"  [yellow]Would attach cloud-init drive on {ci_storage}[/yellow]")
else:
    ci_cmd = ["qm", "set", str(vmid), "--ide0", f"{ci_storage}:cloudinit"]
    ssh_cmd = ["ssh", f"root@{proxmox_host}"] + ci_cmd
    subprocess.run(ssh_cmd, check=True)
    console.print(f"  [green]Cloud-init drive attached[/green]")
```

- [x] **Step 5: Pass cloud-init IP config in qm create**

In the `build_qm_create_command` call, the `cloud_init_snippet` is already `None` which is fine — `--ipconfig0` is now always set (from Task 3). No change needed here, but verify the network_config dict has `address`, `prefix`, and `gateway` keys (it does, from the existing code).

- [x] **Step 6: Pass force_rebuild_iso to ensure_iso_on_proxmox**

Update the `iso_installer.ensure_iso_on_proxmox` call to pass force:

```python
iso_ref = iso_installer.ensure_iso_on_proxmox(
    proxmox_host=proxmox_host,
    storage="local",
    build_if_missing=True,
    force=force_rebuild_iso,
    verbosity=1,
)
```

- [x] **Step 7: Remove steps 9-11 (secrets, nixos-anywhere, cleanup)**

Remove everything after the "Waiting for VM to get an IP address" step (after confirming SSH is accessible). This means removing:
- Step 9 (preparing secrets, lines 414-425)
- Step 10 (running nixos-anywhere, lines 427-445)
- Step 11 (cleanup and reboot, lines 447-467)

Replace the ending with:

```python
console.print("\n[bold green]Done![/bold green]")
if not dry_run:
    console.print(f"\nVM VMID: {vmid}")
    console.print(f"VM IP: {ip_alloc['address']}")
    console.print(f"\nNext: clan machines install {machine} root@{ip_alloc['address']}")
```

- [x] **Step 8: Commit**

```bash
git add tools/ncf/ncf/commands/provision_vm_anywhere.py
git commit -m "feat(provision): simplify provision-vm-anywhere, remove nixos-anywhere steps"
```

---

## Task 6: Update CLI flags

**Files:**
- Modify: `tools/ncf/ncf/cli.py:789-836`

- [x] **Step 1: Update command parameters**

Replace the `machine_provision_vm_anywhere_cmd` function parameters and body:

Remove:
```python
no_generate_hardware_config: bool = typer.Option(
    False,
    "--no-generate-hardware-config",
    help="Skip hardware config generation (use existing)",
),
```

Add:
```python
force_rebuild_iso: bool = typer.Option(
    False,
    "--force-rebuild-iso",
    "-f",
    help="Force rebuild and re-upload the installer ISO",
),
```

Update the function call to `provision_vm_anywhere.run()`:
```python
provision_vm_anywhere.run(
    machine=machine,
    proxmox_host=proxmox_host,
    bridge=bridge,
    ssh_timeout=ssh_timeout,
    force_rebuild_iso=force_rebuild_iso,
    dry_run=dry_run,
)
```

Update the docstring to remove mention of nixos-anywhere and add cloud-init/clan install info.

- [x] **Step 2: Commit**

```bash
git add tools/ncf/ncf/cli.py
git commit -m "feat(cli): update provision-vm-anywhere flags"
```

---

## Task 7: Refactor git-crypt as flake package (added during implementation)

**Files:**
- Modify: `modules/devshell.nix`
- Modify: `modules/interactive-cli.nix`

git-crypt worktree support patch was already in `interactive-cli.nix` but
missing from `devshell.nix`. Worktrees created via `nix develop` couldn't
unlock git-crypt.

- [x] **Step 1: Define `git-crypt-patched` in devshell.nix let block**
- [x] **Step 2: Expose as `packages.git-crypt-patched`**
- [x] **Step 3: Reference via `self'.packages.git-crypt-patched` in interactive-cli.nix**
- [x] **Step 4: Commit**

---

## Task 8: Verify end-to-end

- [x] **Step 1: Run help check**

Run: `nix run .#ncf -- machine provision-vm-anywhere --help`

Verify the output shows:
- `--force-rebuild-iso` / `-f` flag present
- `--no-generate-hardware-config` removed
- Docstring mentions cloud-init and clan install

---

## Lessons Learned

- **Always run `nix run`/`nix eval` from the worktree directory.** Running from
  the main repo uses the main repo's flake source, not the worktree's.
- **git-crypt worktree patch must be in devshell too.** Without it, worktrees
  created via `nix develop` can't checkout encrypted files. Refactored as a
  shared flake package to avoid duplication.
- **Proxmox cloud-init needs an explicit drive.** `--ipconfig0` alone doesn't
  create a cloud-init drive. Must also run `qm set --ide0 <storage>:cloudinit`.
