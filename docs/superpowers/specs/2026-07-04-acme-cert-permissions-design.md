# ACME client cert permissions + reloadServices

## Problem

PostgreSQL on the `postgres` machine fails to start SSL:

```
FATAL: could not load server certificate file "/var/lib/ssl-cert/full.pem": Permission denied
```

The `lets-encrypt` clan service (`modules/clan/acme.nix`) `roles.client` distributes a
combined cert+key PEM to `/var/lib/ssl-cert/full.pem`, but it is hard-wired for an
nginx consumer and does not manage permissions for other consumers.

Three concrete defects:

1. **Directory traversal.** `/var/lib/ssl-cert` is created `acme-puller:nginx 0750`.
   The `postgres` user is neither `acme-puller` nor in the `nginx` group, so it cannot
   traverse the directory — the exact cause of the error.

2. **Unsafe file permissions.** `pull-acme-cert.service` runs as **root** with the
   default umask, so both the self-signed **stub** (created before the first successful
   download) and the **downloaded/decrypted** cert land as `root:root 0644`. The
   combined PEM contains the **private key**, so it is world-readable — a security hole,
   and PostgreSQL refuses any key file with group/world access.

3. **Dead `reloadServices` setting.** `modules/machines/postgres.nix` and
   `modules/machines/metabase.nix` pass `reloadServices` into the acme client settings,
   but the interface does not declare it and nothing consumes it. Clan's freeform
   settings silently swallow it, so consumers are never reloaded when a cert rotates,
   and the pull service never orders itself before non-nginx consumers.

## Approach

Manage cert permissions the same way clan vars secrets are managed: expose explicit
`owner` / `group` / `mode` options on the PEM and enforce them consistently on both the
stub and the downloaded file. Multiple consumers on one machine are supported implicitly
by **group ownership** (each consumer joins the chosen group) rather than by emitting
multiple copies. Wire up `reloadServices` for both startup ordering and reload-on-rotate.

Rejected alternatives:
- Shared magic `ssl-cert` group created by the module — less explicit, unlike the rest
  of the codebase.
- Multiple output copies, one per consumer — no current machine needs it (YAGNI).

## Design

### 1. Interface (`roles.client`, `modules/clan/acme.nix`)

Add options alongside `domain` / `extraDomainNames`:

| option | type | default | purpose |
|--------|------|---------|---------|
| `owner` | str | `"root"` | owner of `full.pem` |
| `group` | str | `"nginx"` | group of `full.pem` and of the `/var/lib/ssl-cert` dir |
| `mode` | str | `"0640"` | mode of `full.pem` |
| `reloadServices` | listOf str | `[ ]` | units ordered after, and reloaded on cert change |

Defaults preserve today's behavior for existing nginx machines (web, social, metabase,
docker-on-nixos): the nginx master reads certs as root at startup, and the dir group
stays `nginx`. The `mode` default of `0640` (vs the current de-facto `0644`) additionally
closes the world-readable-private-key hole for all machines.

### 2. Directory

The `/var/lib/ssl-cert` tmpfiles rule changes its group from the hard-coded `nginx` to
`settings.group`, staying `acme-puller:<group> 0750`. With `group = "postgres"`, the
postgres user traverses the directory via group `x`.

Mode stays `0750` — **no `o+x`**. Traversal is granted through group membership; the file
is `0640` so a non-group local user could not read the key anyway, and `o+x` would only
leak directory `stat`/enumeration for no benefit.

### 3. `pull-acme-cert` script — enforce perms on both paths

Rewrite the script body so that, regardless of which branch runs, the final perms are
always normalized:

- **Stub branch** (`full.pem` missing): unchanged self-signed generation.
- **Download/decrypt branch**: `curl` the encrypted blob, `age --decrypt` it, and only
  `mv` it into place when it actually differs from the current file (`cmp -s`). Track a
  `changed` flag. Clean up `new-full.pem` / `new-full-decrypted.pem` temp files.
- **Unconditional tail**: `chown <owner>:<group> full.pem` then `chmod <mode> full.pem`,
  so every run — stub, download, or no-op — leaves correct ownership/mode.
- If `changed`, `systemctl try-reload-or-restart <reloadServices…>` (a no-op for units
  not yet active, e.g. first boot before consumers start).

Resulting perms for postgres: `root:postgres 0640`, which satisfies PostgreSQL's key
check (owned by root, group-readable, no group-write/exec, no world access) with the
postgres user in the `postgres` group.

### 4. Wire up `reloadServices`

- **Ordering**: `requiredBy` and `before` become
  `[ "nginx.service" ] ++ settings.reloadServices` (deduped). Keeping `nginx.service` as a
  default preserves startup ordering for web/social, which declare no `reloadServices`;
  on the postgres machine the nginx units are simply inactive and harmless. This makes
  `pull-acme-cert` run **before** `postgresql.service`, so a stub exists before postgres
  starts.
- **Reload-on-rotate**: handled in the script tail above.

### 5. Machine change

`modules/machines/postgres.nix`: add `group = "postgres";` to the acme client settings.
`owner`/`mode` take the new defaults; `reloadServices = [ "postgresql.service" ]` is
already present and becomes live.

## Post-deploy step

Directory permissions apply at activation (tmpfiles), but `full.pem` is re-permed by
`pull-acme-cert`, which does not auto-run on `nixos-rebuild switch`. After deploying,
run `systemctl start pull-acme-cert` on the postgres machine once (or reboot) to
normalize the existing `full.pem`, then confirm postgres starts.

## Verification

- `full.pem` on postgres is `root:postgres 0640`; `/var/lib/ssl-cert` is
  `acme-puller:postgres 0750`.
- `sudo -u postgres cat /var/lib/ssl-cert/full.pem` succeeds; postgres starts with SSL.
- Existing nginx machines (web/social/metabase) still serve TLS after a rebuild; their
  `full.pem` is now `root:nginx 0640` rather than world-readable `0644`.
- A cert rotation (or manual `pull-acme-cert` run that changes the file) triggers a
  reload of the declared `reloadServices`.

## Out of scope

- Multiple output copies per machine.
- Module-managed creation of shared consumer groups.
