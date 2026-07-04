# hledger PostgreSQL database + role-based grants in the clan module — design

## Problem

We need a new PostgreSQL database `hledger` on the `postgres` host with three
distinct roles:

- an **owner** that creates and owns the schema/tables,
- a **read-write** role (DML on all tables + sequences), and
- a **read-only** role (SELECT only),

all reachable from anywhere on the tailnet over SSL.

The existing clan module `flake.clan.modules.postgresql`
(`modules/clan/postgresql.nix`) already provisions `(database, user)` pairs with
shared passwords, pg_hba lines, and SSL — but it only understands a single
`owner` boolean per entry. Any finer-grained privileges (a read-write vs
read-only split) currently have to be hand-written as raw SQL in each consumer's
`initSql` field. That does not generalize and puts database-specific SQL in
machine modules.

**Goal:** move grant generation *into the clan module* as reusable privilege
templates, then declare `hledger` using them. No hand-written GRANT SQL in
`postgres.nix`.

## Key facts that shape the design

- **No dedicated hledger consumer machine exists.** hledger data is accessed
  directly via `psql` over the tailnet, not by an app on its own host. So the
  three `(hledger, *)` access entries are attached to the **`postgres` machine
  acting as its own `client`** (the postgres machine already joins the `postgres`
  instance as `server`; it additionally joins as `client`). The auto-generated
  passwords therefore deploy onto the postgres box itself
  (`/run/secrets/...`, `root:root 0400`) — which is also where you read them to
  hand out to clients.

- **`initSql` runs against the wrong database by default.** The provisioning
  oneshot runs `runuser -u postgres -- psql -f <rendered.sql>` with **no `-d`**,
  so it connects to the default database, not the target one. Schema-local
  statements (`ALTER SCHEMA`, `GRANT ... ON ALL TABLES IN SCHEMA`,
  `ALTER DEFAULT PRIVILEGES ... IN SCHEMA`) must therefore be preceded by a
  `\connect "<database>"` psql meta-command. The module will emit this once, at
  the top of every generated grant block.

- **PostgreSQL 15+ locks down `public`.** Database ownership does *not* confer
  the right to create objects in the `public` schema (owned by the bootstrap
  superuser). The owner role must be given schema-level rights explicitly. We do
  this with `ALTER SCHEMA public OWNER TO "<owner>"` — appropriate for these
  single-app databases, and it makes `ALTER DEFAULT PRIVILEGES FOR ROLE <owner>`
  the natural way to cover future tables.

- **Roles exist before any grant runs.** `clan.core.postgresql` creates every
  user and database during `postgresql.service` startup, before the provisioning
  oneshots fire. So a grant block that references all three roles (owner, rw, ro)
  is safe regardless of the order the per-entry oneshots run in.

## Approach

Extend `modules/clan/postgresql.nix` with a per-entry **`role` enum** that selects
a privilege template the module renders into the provisioning SQL. Then declare
`hledger` in `modules/machines/postgres.nix` using those roles, and migrate the
one existing consumer (`metabase`) from the old `owner` boolean to the new enum.

### Interface change: `owner` (bool) → `role` (enum)

On each `settings.access.<label>` entry, **remove** `owner` and **add**:

```nix
role = lib.mkOption {
  type = lib.types.enum [ "owner" "readwrite" "readonly" "none" ];
  default = "none";
  description = ''
    Privilege template auto-applied to this role on its database:
      owner     - ALTER DATABASE ... OWNER + ALTER SCHEMA public OWNER (can create tables)
      readwrite - CONNECT, USAGE, SELECT/INSERT/UPDATE/DELETE + sequences, incl. future tables
      readonly  - CONNECT, USAGE, SELECT, incl. future tables
      none      - no auto grants; use initSql yourself
    initSql is always appended after the generated grants.
  '';
};
```

`none` is the default, preserving today's behaviour for any entry that used the
old `owner = false` (a bare role + password + pg_hba line, privileges via
`initSql`). `initSql` remains an escape hatch, appended verbatim after any
generated grants.

### Grant generation in the module

A pure helper `grantSql entry ownerUser` returns the SQL for an entry's `role`.
Each non-`none` block opens with `\connect "<database>"` so schema-local
statements target the right database:

- **owner**
  ```sql
  \connect "<db>"
  ALTER SCHEMA public OWNER TO "<user>";
  ```
  (The `ALTER DATABASE "<db>" OWNER TO "<user>"` line is still emitted separately,
  as it is today, gated on `role == "owner"`; it runs in the default-db context
  before the `\connect`, which is correct for `ALTER DATABASE`.)

- **readwrite**
  ```sql
  \connect "<db>"
  GRANT CONNECT ON DATABASE "<db>" TO "<user>";
  GRANT USAGE ON SCHEMA public TO "<user>";
  GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO "<user>";
  GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO "<user>";
  ALTER DEFAULT PRIVILEGES FOR ROLE "<ownerUser>" IN SCHEMA public
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO "<user>";
  ALTER DEFAULT PRIVILEGES FOR ROLE "<ownerUser>" IN SCHEMA public
    GRANT USAGE, SELECT ON SEQUENCES TO "<user>";
  ```

- **readonly**
  ```sql
  \connect "<db>"
  GRANT CONNECT ON DATABASE "<db>" TO "<user>";
  GRANT USAGE ON SCHEMA public TO "<user>";
  GRANT SELECT ON ALL TABLES IN SCHEMA public TO "<user>";
  ALTER DEFAULT PRIVILEGES FOR ROLE "<ownerUser>" IN SCHEMA public
    GRANT SELECT ON TABLES TO "<user>";
  ```

- **none** → empty string.

`ownerUser` is the single `role == "owner"` user for the entry's database,
computed by the module from the flattened entries across **all** client machines
(`ownerByDb`). This means a `readwrite`/`readonly` entry's
`ALTER DEFAULT PRIVILEGES FOR ROLE <owner>` names the correct owner even if the
owner is declared on a different client machine.

`grantSql` must **guard on a missing owner** (e.g. emit the `ALTER DEFAULT
PRIVILEGES` lines only when `ownerUser != null`) rather than interpolate a `null`
into the SQL string. Otherwise a database with a rw/ro entry but no owner would
throw an opaque "cannot coerce null to string" eval error and mask the friendly
"exactly one owner per database" assertion message. The assertion is the intended
surfaced error for that misconfiguration.

Every statement is idempotent (`GRANT`, `ALTER SCHEMA ... OWNER`,
`ALTER DEFAULT PRIVILEGES`), so re-running the oneshot on each deploy is a no-op
once applied.

### Template wiring

The `sops.templates` content builder (currently `postgresql.nix:193-199`) becomes:

```nix
content = ''
  ALTER USER "${e.user}" WITH PASSWORD '${config.sops.placeholder."${secretName ...}"}';
  ${lib.optionalString (e.role == "owner") ''ALTER DATABASE "${e.database}" OWNER TO "${e.user}";''}
  ${grantSql e (ownerByDb.${e.database} or null)}
  ${e.initSql}
'';
```

`flattenClients` (`postgresql.nix:11-30`) is updated to carry `role` instead of
`owner`. `ownersByDb` (`postgresql.nix:152`) is redefined in terms of
`role == "owner"`, and a scalar `ownerByDb` (db → single owner user) is derived
from it for `grantSql`.

### Assertion: exactly one owner per database

Replace the current "≤1 owner" assertion with a stronger, single rule evaluated
per database referenced by any access entry:

> Each provisioned database must have **exactly one** `role = "owner"` entry.

- **zero owners** → fail eval: a database with rw/ro/none roles but no owner has
  no role to anchor `ALTER SCHEMA ... OWNER` / `ALTER DEFAULT PRIVILEGES FOR
  ROLE <owner>`.
- **more than one owner** → fail eval (as today).

The message names the offending database and the owner count so the fix is
obvious.

## Consumer changes

### `modules/machines/metabase.nix`

`clan.inventory.instances.postgres.roles.client.machines.metabase.settings.access.metabase`:
change `owner = true;` → `role = "owner";`. This is the only consumer of the
`postgres` instance today, so it is the entire migration.

**Behaviour change (intentional, safe):** metabase's owner entry now additionally
runs `ALTER SCHEMA public OWNER TO "metabase"`. This is purely additive (grants,
never revokes), idempotent, and only strengthens metabase's ability to manage its
own schema. No other metabase SQL changes.

### `modules/machines/postgres.nix`

Add the three hledger entries under the postgres machine's own `client` role —
pure declaration, zero hand-written SQL:

```nix
clan.inventory.instances.postgres.roles.client.machines.postgres.settings.access =
  let
    tsCIDRs = [ "100.64.0.0/10" "fd7a:115c:a1e0::/48" ];
  in
  {
    hledger    = { database = "hledger"; user = "hledger";    role = "owner";     sourceCIDRs = tsCIDRs; };
    hledger-rw = { database = "hledger"; user = "hledger_rw"; role = "readwrite"; sourceCIDRs = tsCIDRs; };
    hledger-ro = { database = "hledger"; user = "hledger_ro"; role = "readonly";  sourceCIDRs = tsCIDRs; };
  };
```

Notes:
- `database`/`user` are set explicitly on every entry because the `access` label
  otherwise defaults both to the label (e.g. label `hledger-rw` would mean db
  `hledger-rw`).
- `sourceCIDRs` covers the whole tailnet: `100.64.0.0/10` (tailscale IPv4 CGNAT)
  and `fd7a:115c:a1e0::/48` (tailscale IPv6 ULA). The module emits one
  `hostssl hledger <user> <cidr> scram-sha-256` line per (user × CIDR) → 6 lines.
- `restartUnits` is left at its default `[]` — nothing on the postgres box needs
  restarting when these passwords change.
- Passwords are auto-generated shared vars (one per role), deployed to the
  postgres host at `/run/secrets/vars/postgresql-postgres-hledger-<user>/password`
  (`root:root 0400`). Read them there to configure clients.

## What the module still provides for free

Unchanged from the existing module, no new work:

- Idempotent `CREATE USER` / `CREATE DATABASE` for all three roles + the `hledger`
  database (via `clan.core.postgresql`).
- One 32-byte `openssl rand -hex 32` shared password per `(database, user)` pair,
  never written to `/nix/store` or `psql` argv.
- pg_hba `hostssl ... scram-sha-256` lines from `sourceCIDRs`.
- SSL (`ssl = on`, cert from the acme client role) and `listen_addresses = "*"`.

## Known limitation (documented, not worked around)

Because the postgres machine is **both** `server` and `client` for these entries,
the two shared-password generators the module would produce for each role (one via
the server code path, one via the client code path) collapse to a single attr in
`lib.listToAttrs` — the client definition wins. Consequence: the generator's
`restartUnits` link to the provisioning oneshot is dropped for the self-client
case. Impact is limited to password *rotation*: a plain re-deploy re-runs the
oneshot (it is `wantedBy = multi-user.target`), so rotated passwords are still
applied; only the automatic "restart the oneshot the instant the var changes"
convenience is lost. First-time provisioning is unaffected. We accept this and
note it rather than complicate the module for a self-referential edge case.

## Error handling & safety

- Secrets flow only through clan vars + sops templates rendered to tmpfs; never
  `/nix/store`, never `psql` argv (unchanged).
- All generated SQL is idempotent; provisioning units are `oneshot` +
  `RemainAfterExit`; `psql` runs with `ON_ERROR_STOP=1` so a genuine failure
  surfaces loudly.
- Build-time assertion enforces exactly one owner per database.

## Testing

- Eval: `nix flake check` and build both `nixosConfigurations.postgres` and
  `nixosConfigurations.metabase` — the interface change must not break metabase.
- `nix fmt` on all touched `.nix` files (repo convention).
- Post-deploy manual verification from a tailnet peer:
  - `psql "postgres://postgres.lynx-lizard.ts.net/hledger?user=hledger_ro&sslmode=require"`
    → `SELECT` succeeds, `INSERT` denied.
  - same as `hledger_rw` → writes succeed.
  - same as `hledger` (owner) → `CREATE TABLE t(...)` succeeds; then confirm
    `hledger_rw`/`hledger_ro` immediately see and can query `t` (proves
    `ALTER DEFAULT PRIVILEGES` covers future tables).

## Files touched

- **`modules/clan/postgresql.nix`** — `role` enum replaces `owner`; `grantSql`
  helper; `ownerByDb` derivation; template content builder; updated assertion;
  `flattenClients` carries `role`.
- **`modules/machines/metabase.nix`** — `owner = true` → `role = "owner"`.
- **`modules/machines/postgres.nix`** — add the three hledger `access` entries.
