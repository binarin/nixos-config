# PostgreSQL user/password clan module — design

## Problem

The current postgres + metabase setup wires DB user/password sharing by hand:

- A shared clan vars generator `metabase-db` (`modules/machines/metabase-db.nix`),
  imported by both the `postgres` and `metabase` machine modules.
- A hand-written `metabase-db-password` oneshot on the server that runs
  `ALTER USER metabase WITH PASSWORD ...` reading the shared secret at runtime.
- Hardcoded `services.postgresql.authentication` (pg_hba) lines in `postgres.nix`.
- Client-side `LoadCredential` pointing at the shared secret file.

This is bespoke and does not generalize to more databases/consumers. We want a
proper **clan service module** (like `modules/clan/acme.nix`) that formalizes the
pattern into `server`/`client` roles.

**Hard constraint:** the password must never reach `/nix/store` (world-readable).
Secrets only ever flow through the clan secret store and runtime-read files /
systemd credentials — never interpolated into Nix strings, `initialScript`, or
store paths.

## Approach

A new clan service module `flake.clan.modules.postgresql` in
`modules/clan/postgresql.nix`, `_class = "clan.service"`, with two roles:

- **`server`** — runs on the postgres host. Builds on `clan.core.postgresql`
  (from clan-core) for idempotent CREATE USER/DATABASE + backup/restore hooks,
  and adds: shared password generation, runtime password setting, per-client
  pg_hba entries, SSL/listen config.
- **`client`** — runs on each consuming machine. Owns the shared password
  generator (so the same secret is decrypted on both sides) and exposes the
  password file path. Deliberately thin: share the secret, surface its path.

**Instance model:** a single instance named `postgres`; the postgres host joins
`server`, each consumer joins `client`.

### Secret-sharing mechanism (key design choice)

A DB password is a *symmetric* secret — both server and client need the same
value. We use a clan **`share = true` vars generator** (as the clan-core `users`
service does), co-defined by both roles with an identical name and script.
clan keys shared generators globally by name and deploys the same decrypted
value to every machine that defines it. No runtime distribution, no encryption
dance.

Alternatives rejected:

- *acme-style runtime distribution* (server generates → encrypts per client →
  client pulls over HTTP+age): only justified for frequently-rotating runtime
  material like TLS certs. DB passwords are static. Overkill.
- *server-only generator, publish to client*: impossible — `getPublicValue` /
  exports carry only non-secret data.

## Module shape & files

- **New:** `modules/clan/postgresql.nix` → `flake.clan.modules.postgresql`
  (`_class = "clan.service"`, `manifest`, `roles.server`, `roles.client`).
- Consumed via
  `clan.inventory.instances.postgres.module = { input = "self"; name = "postgresql"; }`
  (same wiring style as the existing `acme-metabase` instance).
- **Retired** by the migration:
  - `modules/machines/metabase-db.nix` (the `metabase-db-generator` module) —
    deleted.
  - The hand-written `clan.core.postgresql` block, `metabase-db-password`
    service, and hardcoded `authentication` lines in `postgres.nix`.
  - The shared `metabase-db` var.

## `server` role

### Interface (per instance, on the server machine)

- `certPath` (str, default `/var/lib/ssl-cert/full.pem`) — combined PEM the acme
  client role already deploys. Server sets `ssl = "on"`,
  `ssl_cert_file` / `ssl_key_file` to this, and `listen_addresses = "*"`.

Memory tuning (`shared_buffers`, `effective_cache_size`, …) stays OUT — it is
machine-specific and remains in the machine module.

### `perInstance` → `nixosModule` behaviour

Iterate `roles.client.machines` (machine → settings), then each machine's
`settings.access` entries, flattening to a list of `(machine, label, entry)`
with fields `{ database, user, owner, sourceCIDRs, restartUnits, initSql }`.

1. **Build on `clan.core.postgresql`** (imported):
   - `users.<user> = { }` for each distinct user.
   - `databases.<database> = { }` for each distinct database (deduped — a DB
     referenced by multiple users is created once). `OWNER` is set at runtime
     (step 4) rather than via `create.options`, so it also works when the DB
     already exists.
   - Gives idempotent CREATE USER/DATABASE **and** backup/restore hooks for free.

2. **Shared password generator per (db, user)** —
   name `postgresql-${instanceName}-${database}-${user}`, `share = true`,
   `files.password = { secret = true; deploy = true; restartUnits = [ "postgresql-provision-${database}-${user}.service" ]; }`,
   `runtimeInputs = [ pkgs.openssl ]`, script `openssl rand -hex 32 > $out/password`.

3. **pg_hba entries per (db, user)** — for each CIDR in `sourceCIDRs`:
   `hostssl <database> <user> <cidr> scram-sha-256`. All lines across all
   clients are concatenated into `services.postgresql.authentication`.

4. **Provisioning oneshot per (db, user)** —
   `postgresql-provision-<database>-<user>.service`:
   - `Type = "oneshot"`, `RemainAfterExit = true`, runs as **root**,
     `after`/`requires` `postgresql.service`.
   - Connects via `runuser -u postgres -- psql` (clan-core's own pattern), so it
     reads the secret file as root regardless of file perms — no server-side
     file-ownership coupling.
   - Steps:
     1. `ALTER USER "<user>" WITH PASSWORD '<runtime-read secret>'`
     2. if `owner`: `ALTER DATABASE "<database>" OWNER TO "<user>"` (idempotent)
     3. run `initSql` verbatim (GRANTs, etc.)
   - Restarted when its password generator changes (via the generator file's
     `restartUnits`).

## `client` role

### Interface (per instance, on each consuming machine)

`settings.access` is an attrset keyed by an arbitrary label:

```nix
access.<label> = {
  database    = "<str>";        # default = label
  user        = "<str>";        # default = label
  owner       = <bool>;         # default false — does this role own the database?
  sourceCIDRs = [ "<cidr>" ];   # server builds hostssl pg_hba lines from this
  restartUnits = [ "<unit>" ];  # default [] — consumer services restarted on rotation
  initSql     = "<str>";        # default "" — raw SQL escape hatch (GRANTs etc.)
  secret = {                    # ownership/perms of the deployed password file (client-side)
    owner = "root";
    group = "root";
    mode  = "0400";
  };
};
```

This supports a single machine holding **multiple (database, user) pairs**,
including **different users on the same database**.

### `perInstance` → `nixosModule` behaviour

For each `access.<label>` entry:

1. **Define the same shared generator**
   `postgresql-${instanceName}-${database}-${user}` (`share = true`, identical
   script), with the file's `owner`/`group`/`mode` set from `settings.secret` and
   `restartUnits` from `settings.restartUnits`, `deploy = true` — so the secret
   lands on the client machine, is readable by a direct-read consumer, and
   rotation restarts the consumer.
2. **Expose the password path only.** No `LoadCredential`/env wiring; the
   consumer references
   `config.clan.core.vars.generators."postgresql-${instanceName}-${database}-${user}".files.password.path`
   itself (as `metabase.nix` does today).

`secret.{owner,group,mode}` matters for consumers that read the file directly as
their own user. Consumers using systemd `LoadCredential` (root reads, then drops
to the service) can leave it at the `root:root:0400` default.

## Error handling & safety

- Secrets read at runtime only — never `/nix/store`.
- All SQL is idempotent (clan-core `SELECT 1 ... || ...` style); provisioning
  units are `oneshot` + `RemainAfterExit`.
- Password rotation = regenerate the shared var → server provisioning oneshot
  restarts (server side) and consumer `restartUnits` restart (client side).
- **Build-time assertion:** two access entries both `owner = true` on the same
  database is a configuration error and fails evaluation.

## Testing

- `nix flake check` / eval of `nixosConfigurations.postgres` and `.metabase`.
- `nix fmt` on all touched `.nix` files (per repo convention, commit `f44298eb`).
- No VM test unless desired.

## Migration cutover (instance `postgres`)

1. Add `flake.clan.modules.postgresql` in `modules/clan/postgresql.nix`.
2. `modules/machines/postgres.nix`: drop the hand-written `clan.core.postgresql`
   block, the `metabase-db-password` service, and the hardcoded `authentication`
   lines; keep memory tuning + LXC; join `server` role via the `postgres`
   instance.
3. `modules/machines/metabase.nix`: join `client` role with
   `access.metabase = { owner = true; sourceCIDRs = [ "100.64.0.0/10" "192.168.2.36/32" ]; restartUnits = [ "metabase.service" ]; }`;
   point `LoadCredential` at the module's generator path.
4. Delete `modules/machines/metabase-db.nix`.

The `metabase` user/DB and password already exist on disk. The new generator
name differs from the old `metabase-db`, so the first deploy generates and sets a
fresh password — safe because server and client rotate together.

## Implementation risk to verify

The same `share = true` generator is co-defined by both the server (in its
per-client loop) and the owning client. clan keys shared generators globally by
name; the clan-core `users` service shows per-machine file metadata (e.g.
`restartUnits`, and here `owner`/`group`/`mode`) may differ across machines for
one shared generator. Confirm during the build that co-defining from both roles
does not trip a consistency check. Fallback: extract a small shared NixOS module
(like today's `metabase-db-generator`), parametrized by `(instanceName, database,
user)`, imported by both roles.
