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
`modules/clan/postgresql.nix`, `_class = "clan.service"`, with two roles plus a
`perMachine` block:

- **`server`** (role) — runs on the postgres host. Builds on `clan.core.postgresql`
  (from clan-core) for idempotent CREATE USER/DATABASE + backup/restore hooks,
  and adds: runtime password setting, per-client pg_hba entries, SSL/listen
  config.
- **`client`** (role) — runs on each consuming machine. Exposes the password file
  path. Deliberately thin: surface the shared secret's path.
- **`perMachine`** — declares the shared password generators (see the
  Secret-sharing section). Both roles only *reference* them.

**Instance model:** a single instance named `postgres`; the postgres host joins
`server`, each consumer joins `client`.

### Secret-sharing mechanism (key design choice)

A DB password is a *symmetric* secret — both server and client need the same
value. We use a clan **`share = true` vars generator**: clan keys shared
generators globally by name and deploys the same decrypted value to every machine
that declares it. No runtime distribution, no encryption dance.

**Where the generator is declared:** in the module's **`perMachine`** block, not
in either role's `perInstance`. This is the pattern the clan-core `certificates`
and `zerotier` services use for shared secrets. `perMachine` runs once per machine
in the instance (across all roles) and receives `{ instances, machine, ... }`, so
the generator is written in a **single code path** and evaluated per machine —
eliminating any risk of two roles co-defining the same shared generator with
drifting scripts. Roles' `perInstance` blocks then merely reference
`config.clan.core.vars.generators.<name>.files.password.path`.

Within `perMachine`, branch on `machine.roles`:

- **server machine** (`builtins.elem "server" machine.roles`) → declare the
  generator for *every* `(database, user)` across all clients (it must read them
  all to set passwords).
- **client machine** → declare only that machine's own `access` entries'
  generators (least privilege — the secret lands only where needed).

Per-machine file metadata (`restartUnits`, `secret.{owner,group,mode}`) is
computed in that same block from the machine's role/settings — exactly how
`zerotier`'s `perMachine` varies `restartUnits` per machine.

Alternatives rejected:

- *acme-style runtime distribution* (server generates → encrypts per client →
  client pulls over HTTP+age): only justified for frequently-rotating runtime
  material like TLS certs. DB passwords are static. Overkill.
- *server-only generator, publish to client*: impossible — `getPublicValue` /
  exports carry only non-secret data.

## Module shape & files

- **New:** `modules/clan/postgresql.nix` → `flake.clan.modules.postgresql`
  (`_class = "clan.service"`, `manifest`, `roles.server`, `roles.client`,
  `perMachine`).
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

2. **References the shared password generators** declared in `perMachine` (see
   below) — `config.clan.core.vars.generators."postgresql-${instanceName}-${database}-${user}".files.password.path`.
   On the server machine, `perMachine` sets that file's
   `restartUnits = [ "postgresql-provision-${database}-${user}.service" ]`.

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

The client role is thin — the generators themselves are declared in `perMachine`
(which, on a client machine, sets each generator file's `owner`/`group`/`mode`
from that entry's `settings.secret`, `restartUnits` from `settings.restartUnits`,
and `deploy = true`, so the secret lands on the client, is readable by a
direct-read consumer, and rotation restarts the consumer).

The role therefore just **exposes the password path** — no `LoadCredential`/env
wiring. The consumer references
`config.clan.core.vars.generators."postgresql-${instanceName}-${database}-${user}".files.password.path`
itself (as `metabase.nix` does today).

`secret.{owner,group,mode}` matters for consumers that read the file directly as
their own user. Consumers using systemd `LoadCredential` (root reads, then drops
to the service) can leave it at the `root:root:0400` default.

## `perMachine` (shared generator declaration)

`perMachine` receives `{ instances, machine, ... }`. From `instances` it computes
the full set of `(instanceName, database, user, entry)` tuples in the instance
(flattening every client's `access` map). Then, keyed by generator name
`postgresql-${instanceName}-${database}-${user}`, it declares — for the tuples
relevant to `machine`:

- `share = true`, `files.password.secret = true`, `files.password.deploy = true`,
  `runtimeInputs = [ pkgs.openssl ]`, script `openssl rand -hex 32 > $out/password`.
- File metadata computed per machine:
  - **server machine** (`builtins.elem "server" machine.roles`): declares *all*
    tuples; `files.password.restartUnits = [ "postgresql-provision-${database}-${user}.service" ]`.
    (Ownership left at the `root:root:0400` default — the provisioning oneshot
    reads as root.)
  - **client machine**: declares only the tuples whose `access` entry belongs to
    this machine; `files.password.{owner,group,mode}` from that entry's
    `settings.secret`, `restartUnits` from `settings.restartUnits`.

Because the generator body (name/script/`files` set) is one expression evaluated
per machine, the shared definition never drifts; only per-machine deploy metadata
varies.

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

## Shared-generator placement (resolved)

An earlier draft co-defined the `share = true` generator separately in both the
`server` and `client` roles, which risked the two definitions drifting. Inspecting
clan-core resolved this: `certificates` and `zerotier` both declare their shared
generators in **`perMachine`**, never per-role. We follow that pattern (see the
Secret-sharing section) — one definition, evaluated per machine, branching on
`machine.roles`. No co-definition, so there is no drift risk to verify.
