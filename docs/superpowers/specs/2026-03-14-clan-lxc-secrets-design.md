# Clan-aware LXC Secret Provisioning

## Summary

Extend ncf's LXC provisioning to support clan-based machines. For clan machines, secrets are gathered using clan_lib's `SecretStore.populate_dir()` instead of the existing SSH host key injection. The existing non-clan code path remains unchanged.

## Detection

`is_clan_machine(machine_name)` checks whether a machine is managed by clan using `clan_lib.machines.actions.list_machines()` with a `Flake` object pointing to the repo root.

## Secret Gathering

For clan machines, `gather_secrets_for_clan_machine(machine_name)`:

1. Creates a `Flake` object from the repo root
2. Creates a `SecretStore` from the flake
3. Gets generators via `get_machine_generators()`
4. Gets the upload directory via `SecretStore.get_upload_directory()` (typically `/var/lib/sops-nix`)
5. Calls `SecretStore.populate_dir()` with phases `["activation", "users", "services"]` to a temp directory
6. Walks the output directory, creating `SecretFile` objects where:
   - `target_path` = upload_directory + relative path (e.g., `/var/lib/sops-nix/key.txt`)
   - `source_path` = path to already-decrypted file in temp dir
   - `pre_decrypted = True` (new field)
   - `owner`/`group` = `"root"/"root"`, `mode` = `0o600`

## SecretFile Changes

Add `pre_decrypted: bool = False` field to `SecretFile` dataclass.

## Decryption Changes

`decrypt_secrets_to_tempdir()`: for secrets with `pre_decrypted=True`, copy the file instead of calling sops decrypt.

## Provisioning Changes

`provision_lxc.run()`: skip `restore_ssh_host_keys()` for clan machines (clan manages SSH keys via sops-nix at runtime).

## Packaging Changes

`modules/devshell.nix`: add clan-cli's Python package to ncf's `dependencies` list so `clan_lib` is importable.

## Files Changed

- `tools/ncf/ncf/secrets_inject.py` — clan detection, populate_dir integration, pre_decrypted support
- `tools/ncf/ncf/commands/provision_lxc.py` — skip restore_ssh_host_keys for clan machines
- `modules/devshell.nix` — add clan-cli to ncf dependencies
