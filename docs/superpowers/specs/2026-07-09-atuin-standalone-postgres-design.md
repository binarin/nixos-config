# atuin → standalone PostgreSQL migration

**Date:** 2026-07-09
**Status:** Approved design, ready for implementation plan
**Scope:** Move `media`'s atuin sync server off media's local `services.postgresql` (pg15)
onto the standalone `postgres` machine (pg18, clan `postgresql` service). Immich — the
other local-PG consumer on media — is a **separate, later spec** (see appendix).

## Context

Local-PostgreSQL consumers on `media` today:

| Service   | Backend                                   | In scope? |
|-----------|-------------------------------------------|-----------|
| immich    | local `services.postgresql` (pg15) + VectorChord | phase 2 (separate spec) |
| atuin     | local `services.postgresql` (pg15), `database.createLocally = true` | **this spec** |
| commafeed | embedded H2 file                          | no (not PG) |
| grocy     | SQLite                                    | no (not PG) |
| tandoor   | its own containerized `postgres` (arion `db_recipes`) | no (separate PG) |

The standalone `postgres` machine (`modules/machines/postgres.nix`) runs `postgresql_18`
with the clan `postgresql` service module (`modules/clan/postgresql.nix`). That module
provisions, per client `access.<label>` entry: a role, a database, a `share = true`
password (name `postgresql-<instance>-<database>-<user>`), owner/readwrite/readonly grants,
`hostssl … scram-sha-256` pg_hba lines, and SSL via the machine's acme cert. It needs **no
changes** for atuin (atuin uses no extensions). Established remote-client precedent:
`metabase` and `monitor` both connect to `postgres.lynx-lizard.ts.net:5432`.

atuin needs no VectorChord and no special server setup — this migration is the low-risk
pilot that establishes the reusable "media as a clan-postgres client" pattern before the
immich migration.

## Goals

- atuin server on media reads/writes its data on the standalone pg18 `atuin` database.
- Existing synced history preserved via a manual dump/restore (user-run).
- Password never enters `/nix/store`.
- media stops provisioning a local `atuin` DB. (media's local `services.postgresql`
  remains only until immich migrates in phase 2.)

## Non-goals

- Immich migration (phase 2, separate spec).
- Changing the generic clan `postgresql` module.
- Migrating commafeed/grocy/tandoor (not local-PG consumers).

## Design

### 1. Server — `modules/machines/postgres.nix`

Add `media` as a client of the existing `postgres` clan instance. Under
`clan.inventory.instances.postgres.roles.client`, add:

```nix
machines.media.settings.access = {
  atuin = {
    database = "atuin";
    user = "atuin";
    role = "owner";
    sourceCIDRs = [
      "100.64.0.0/10"
      "fd7a:115c:a1e0::/48"
    ];
  };
};
```

Effect on `postgres`: role `atuin` + database `atuin` created; `share = true` password
`postgresql-postgres-atuin-atuin` generated and applied via `ALTER USER`; `atuin` owns the
DB and the `public` schema; `hostssl atuin atuin <cidr> scram-sha-256` pg_hba lines emitted.
Result: an **empty, atuin-owned** database ready for the restore.

### 2. Client — `modules/machines/media.nix`

**a. Membership.** The `access` block above lists `media`, so media's `perMachine`
`clientGen` gives media the `postgresql-postgres-atuin-atuin` generator (`share = true`),
letting media decrypt the same password the server set.

**b. Env-file generator** (mirrors the existing `tandoor-env` / `sabnzbd-conf` composition
pattern). The password is read from the shared generator and written as a `DB_URI` env var
to a secret file — never into `/nix/store`:

```nix
clan.core.vars.generators.atuin-db-env = {
  dependencies = [ "postgresql-postgres-atuin-atuin" ];
  files.env = {
    secret = true;
    restartUnits = [ "atuin.service" ];
  };
  script = ''
    printf 'ATUIN_DB_URI=postgresql://atuin:%s@postgres.lynx-lizard.ts.net:5432/atuin?sslmode=require\n' \
      "$(cat $in/postgresql-postgres-atuin-atuin/password)" > $out/env
  '';
};
```

- Address `postgres.lynx-lizard.ts.net:5432` matches the `metabase`/`monitor` precedent and
  the tailscale `sourceCIDRs`; media's source IP over tailscale falls in `100.64.0.0/10`.
- `sslmode=require` encrypts without CA pinning; the server enforces `hostssl … scram-sha-256`.

**c. Point atuin at the standalone DB:**

```nix
services.atuin.database.createLocally = false;
services.atuin.database.uri = lib.mkForce null;   # module then emits no ATUIN_DB_URI;
                                                    # the env-file is the sole source
services.atuin.environmentFile =
  config.clan.core.vars.generators.atuin-db-env.files.env.path;
```

Rationale for `uri = mkForce null`: the atuin module only sets `ATUIN_DB_URI` from
`database.uri` when non-null; forcing null guarantees the value comes solely from the
`environmentFile`, avoiding any systemd `Environment=` vs `EnvironmentFile=` precedence
ambiguity.

### 3. Data flow (post-cutover)

```
atuin client
  → https://atuin.binarin.info (nginx :443, media)
  → 127.0.0.1:8888  atuin-server (media)
  → tailscale, TLS, scram-sha-256
  → postgres.lynx-lizard.ts.net:5432  database "atuin" (pg18, standalone)
```

Redis: not used by atuin.

## Cutover plan

All shell, deploy, and clan-vars/secret operations are **user-run** (my sops identity can't
decrypt machine secrets); this spec supplies the commands. Ordering matters: atuin must be
**masked** during the window so a deploy can't start it against the empty DB and let it
auto-run sqlx migrations before the restore.

### Commit A — prepared state
- `postgres.nix`: add the `access.atuin` client entry (§1).
- `media.nix`: add the `atuin-db-env` generator + `createLocally = false` +
  `uri = mkForce null` + `environmentFile` (§2), **plus** `systemd.services.atuin.enable = false;`
  (masks the unit → symlink to `/dev/null`).

```bash
clan vars generate                 # creates postgresql-postgres-atuin-atuin + atuin-db-env
<deploy> postgres                  # provisions empty atuin DB/role/password on pg18
<deploy> media                     # atuin masked; existing pg15 atuin DB untouched
```

### Dump (media pg15, atuin quiescent because masked)
```bash
sudo -u postgres pg_dump -Fc -d atuin -f /tmp/atuin.dump
sudo -u postgres psql -d atuin -c 'select count(*) from history;'   # record this count
```

### Restore (standalone pg18, DB empty + atuin-owned)
```bash
# transfer /tmp/atuin.dump to the postgres box, then on postgres:
sudo -u postgres pg_restore --no-owner --no-privileges --role=atuin -d atuin /tmp/atuin.dump
sudo -u postgres psql -d atuin -c 'select count(*) from history;'   # must match media's count
```
`--role=atuin` (SET ROLE at start) makes restored objects atuin-owned; `--no-owner`/
`--no-privileges` skip the dump's ownership/GRANT statements (the clan module already made
atuin own the DB + `public` schema). pg15→pg18 custom-format dumps restore forward-compatibly.

### Commit B — cutover
Delete the `systemd.services.atuin.enable = false;` line, then:
```bash
<deploy> media
systemctl status atuin              # active, no restart loop
journalctl -u atuin -n50            # migrations clean, DB connected
atuin sync                          # from a client — succeeds
```

### Cleanup (optional, after verification)
The old pg15 atuin DB/role linger harmlessly (no longer nix-managed). Drop whenever, or let
them disappear when immich migrates and media's local postgres is removed in phase 2:
```bash
sudo -u postgres psql -c 'DROP DATABASE atuin;'
sudo -u postgres psql -c 'DROP ROLE atuin;'
```

### Rollback (any time before Commit B)
Revert Commit A, `clan vars generate`, redeploy media → atuin returns to local pg15,
untouched.

## Risks / open points

- **Env-file generator dependency on a clan-service `share = true` generator.** The
  `atuin-db-env` generator lists `postgresql-postgres-atuin-atuin` in `dependencies`. This
  is expected to work because the shared generator is instantiated on media via the module's
  `perMachine` `clientGen`. Verify during implementation that `clan vars generate` resolves
  the dependency (mirror of the working `tandoor-secrets` → `tandoor-env` chain).
- **`atuin` role name collision (intended).** media's local role and the standalone role are
  both named `atuin`; ownership references in the dump therefore resolve cleanly on restore.
- **sslmode.** `require` (encrypt, no CA verification) is sufficient given scram auth. If
  stricter verification is later wanted, switch to `verify-full` and ensure media trusts the
  acme CA for `postgres.home.binarin.info`.

---

## Appendix — immich follow-up (NOT in this spec; captured so the research isn't lost)

Immich is the harder phase-2 migration. Key facts gathered:

- media's immich currently runs on **pg15 + VectorChord 1.1.1**. VectorChord 1.1.1 is
  available for **pg18**, so the standalone box can host it; the manual dump/restore is a
  **15 → 18** cross-major migration.
- The immich NixOS module only sets up the vector extension,
  `shared_preload_libraries = [ "vchord.so" ]`, `search_path`, the `CREATE EXTENSION`
  bootstrap (unaccent, uuid-ossp, cube, earthdistance, pg_trgm, vector, vchord),
  `ALTER SCHEMA public OWNER TO immich`, and the reindex-on-vchord-upgrade logic **when
  `database.enable = true`**. With a remote DB (`database.enable = false`) none of it runs —
  it must be reproduced **server-side** on the standalone `postgres` machine.
- `CREATE EXTENSION` / `ALTER EXTENSION … UPDATE` require **superuser**, which only exists on
  the postgres server (immich connects as a plain DB-owner role). So the extension bootstrap
  must live server-side, as `postgres`.
- `REINDEX INDEX face_index|clip_index` is an ordinary statement and runs fine over any psql
  connection; the `\gset`/`\if` conditional is psql client meta-commands. Relocating immich's
  `postgresql-setup` `ExecStartPost` to the postgres server, scoped to the immich DB, is the
  clean approach.
- **Resilient REINDEX** (works on empty/fresh DB and every other state): immich's existing
  guard already skips REINDEX on a fresh DB (`vchord_version_before = ''`). Close the
  remaining gap (extension present but indexes not yet created) with a `to_regclass` guard:

  ```sql
  SELECT (:'vchord_version_before' != '' AND :'vchord_version_before' != :'vchord_version_after'
          AND to_regclass('face_index') IS NOT NULL) AS reindex_face \gset
  \if :reindex_face
    REINDEX INDEX face_index;
  \endif
  -- same pattern for clip_index
  ```

- **immich remote auth**: with a non-socket `database.host`, the module requires a
  `secretsFile` containing `DB_PASSWORD=<pass>` (there's an assertion). Compose it from the
  clan-shared password with an env-file generator, exactly like `atuin-db-env` here.
- **immich client config sketch**: `services.immich.database.enable = false;` plus
  `database.host = "postgres.lynx-lizard.ts.net"`, `database.port = 5432`,
  `database.name = "immich"`, `database.user = "immich"`, and `secretsFile` = the composed
  `DB_PASSWORD` env-file. Keep media's local redis. Mask `immich-server` +
  `immich-machine-learning` during cutover.
- **Server side for immich**: `access.immich` (role `owner`) on the postgres machine, plus
  immich-specific setup — `services.postgresql.extensions = ps: [ ps.pgvector ps.vectorchord ]`,
  `shared_preload_libraries` includes `vchord.so`, `ALTER DATABASE immich SET search_path`
  scoped to the immich DB (not globally, to avoid affecting hledger), and the extension
  bootstrap + resilient REINDEX script run as `postgres` against the immich DB (via the clan
  module's `initSql` or a dedicated setup unit).
