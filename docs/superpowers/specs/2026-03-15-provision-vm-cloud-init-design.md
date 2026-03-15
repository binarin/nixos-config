# Simplify provision-vm-anywhere with Cloud-Init IP Assignment

## Problem

`provision-vm-anywhere` currently runs the full nixos-anywhere installation flow (secrets injection, nixos-anywhere, reboot). This is being replaced by `clan install` which the user runs separately. The command should only create and prepare the VM, confirming it gets the correct IP via cloud-init, then leave it running for the user to `clan install` against.

The installer ISO (`iso.nix`) also lacks cloud-init support, so it can't consume Proxmox's cloud-init IP configuration.

## Changes

### 1. `modules/machines/iso.nix` — Enable cloud-init service

Add `services.cloud-init.enable = true;` so the installer ISO can consume Proxmox-generated cloud-init config (specifically `ipconfig0` for static IP assignment).

### 2. `modules/qemu-guest.nix` — Fix cloud-init storage default

Change `cloudInit.storage` default from `"local"` to `"local-zfs"`.

### 3. `tools/ncf/ncf/commands/provision_vm.py` — Always pass `--ipconfig0`

In `build_qm_create_command`, pass `--ipconfig0` with the static IP configuration unconditionally (not gated on `cloud_init_snippet`).

### 4. `tools/ncf/ncf/commands/provision_vm_anywhere.py` — Simplify

Remove:
- Sops preflight check
- `generate_hardware_config` parameter
- `run_nixos_anywhere` function
- Steps 9-11 (secrets prep, nixos-anywhere, cleanup/reboot)
- Unused imports (`shutil`, `tempfile`, `secrets_inject`)

Add:
- `force_rebuild_iso` parameter (default `False`)
- Cloud-init drive attachment: `qm set <vmid> --ide0 <storage>:cloudinit` after creating VM
- Pass cloud-init storage from `vm_config` to determine storage for the cloud-init drive

Keep:
- Steps 1-7 (metadata, check existing, ensure ISO, create VM, configure disks, attach ISO, start VM)
- Step 8 (wait for guest agent IP) as confirmation cloud-init worked
- Final output showing VM info and SSH command

### 5. `tools/ncf/ncf/commands/iso_installer.py` — Support force rebuild

Add `force` parameter to `ensure_iso_on_proxmox`: when true, rebuild and re-upload even if ISO exists on Proxmox.

### 6. `tools/ncf/ncf/cli.py` — Update CLI flags

Remove `--no-generate-hardware-config` flag from `provision-vm-anywhere` command.
Add `--force-rebuild-iso` / `-f` flag.

## Additional Changes (discovered during implementation)

### 7. `modules/devshell.nix` + `modules/interactive-cli.nix` — Patched git-crypt as flake package

git-crypt doesn't work in git worktrees without an upstream patch
(`AGWA/git-crypt@2da5e00`). This patch was already applied inline in
`interactive-cli.nix` but missing from `devshell.nix`. Refactored to
define `git-crypt-patched` once in `devshell.nix` as a flake package
(`packages.git-crypt-patched`) and reference it via `self'.packages`
in both places.

## Lessons Learned

- **Proxmox cloud-init needs an explicit drive**: `--ipconfig0` alone is not
  enough. You must also attach a cloud-init drive with
  `qm set <vmid> --ide0 <storage>:cloudinit`.
- **git-crypt worktree patch**: The devshell was missing the worktree patch
  that `interactive-cli.nix` already had. Created a shared flake package to
  avoid duplication.
- **nix run in worktrees**: Always run `nix run`/`nix eval` from the worktree
  directory itself. Running from the main repo will use the main repo's flake,
  not the worktree's.

## Out of Scope

- Changes to `provision_vm.py`'s `run()` function (legacy disko-based flow)
- Cloud-init user-data snippets (Proxmox native cloud-init is sufficient)
- Changes to `clan install` workflow
