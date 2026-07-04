# hledger Database + Role-Based Grants Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a `hledger` PostgreSQL database with owner / read-write / read-only roles, reachable from the whole tailnet, by teaching the `postgresql` clan module to generate privilege grants from a per-entry `role` enum.

**Architecture:** Extend `modules/clan/postgresql.nix` so each `access.<label>` entry carries a `role` enum (`owner`/`readwrite`/`readonly`/`none`) instead of an `owner` boolean; a pure `grantSql` helper renders the matching `GRANT` / `ALTER DEFAULT PRIVILEGES` SQL (each block prefixed with `\connect "<db>"`) into the existing sops-template provisioning oneshots. Migrate the one existing consumer (`metabase`) to the enum, then declare `hledger` in `postgres.nix` with the postgres host acting as its own `client`.

**Tech Stack:** Nix (flake-parts, clan, sops-nix), PostgreSQL 18, treefmt/nixfmt.

## Global Constraints

- Secrets flow only through clan vars + sops templates rendered to tmpfs — never `/nix/store`, never `psql` argv.
- All generated SQL must be idempotent (re-run on every deploy as a no-op).
- `role` enum values are exactly `owner`, `readwrite`, `readonly`, `none`; default `none`.
- Each provisioned database must have **exactly one** `role = "owner"` entry (build-time assertion).
- Owner entries take schema ownership via `ALTER SCHEMA public OWNER TO "<owner>"` (Option A — additive, applies to metabase too).
- Tailnet source CIDRs are exactly `[ "100.64.0.0/10" "fd7a:115c:a1e0::/48" ]`.
- Run `nix fmt` on every touched `.nix` file before committing (repo convention).
- Verification in this repo is `nix eval` of rendered config attributes (there is no unit-test framework for these modules); whole-config sanity via `nix eval .#nixosConfigurations.<host>.config.system.build.toplevel` (forces a full evaluation, incl. assertions). Do **not** use `just eval-all`.

---

## File Structure

- `modules/clan/postgresql.nix` — **modify.** The clan service module. Gains the `role` enum option, `ownerByDb` derivation, `grantSql` helper, updated template content, and the exactly-one-owner assertion. Owns all grant-generation logic.
- `modules/machines/metabase.nix` — **modify (1 line).** Migrate the existing `owner = true` access entry to `role = "owner"`. Must land in the same commit as the module change so eval stays green.
- `modules/machines/postgres.nix` — **modify.** Add the three `hledger` `access` entries under the postgres host's own `client` role. Pure declaration, no SQL.

The spec is `docs/superpowers/specs/2026-07-04-hledger-database-design.md`.

**Note on TDD in this repo:** these are declarative Nix modules, not code with a test runner. The "failing test" for each task is a `nix eval` command whose output demonstrates the change is *not yet* present; after implementing, the same command demonstrates the new rendered output. That before/after eval is the test cycle.

---

### Task 1: Role-based grant generation in the clan module (+ metabase migration)

**Files:**
- Modify: `modules/clan/postgresql.nix` (flattenClients ~19-27, access option 72-76, server `let` 148-157, template content 193-199, assertions 231-234)
- Modify: `modules/machines/metabase.nix:34`

**Interfaces:**
- Consumes: nothing new (builds on existing module internals `genName`, `secretName`, `flattenClients`, `entries`, `databases`, `unitName`, `tmplName`).
- Produces:
  - Per-entry option `role : enum [ "owner" "readwrite" "readonly" "none" ]` (default `"none"`), replacing the removed `owner : bool`.
  - Internal `ownerByDb : attrsOf str` (database → single owner user) and `grantSql : entry -> (str|null) -> str`, used by the template builder.
  - Rendered template `postgresql-provision-<db>-<user>.sql` now contains role-appropriate grants.

- [ ] **Step 1: Baseline eval — confirm metabase's owner grant does NOT yet include schema ownership**

Run:
```bash
nix eval --raw '.#nixosConfigurations.postgres.config.sops.templates."postgresql-provision-metabase-metabase.sql".content'
```
Expected: output contains `ALTER DATABASE "metabase" OWNER TO "metabase";` but does **NOT** contain `ALTER SCHEMA public OWNER TO "metabase"`. (This is the pre-change state — the test we will make pass.)

- [ ] **Step 2: `flattenClients` — carry `role` instead of `owner`**

In `modules/clan/postgresql.nix`, the `inherit (entry)` block inside `flattenClients` (~lines 19-27) currently lists `owner`. Replace that block with:

```nix
              inherit (entry)
                database
                user
                role
                sourceCIDRs
                restartUnits
                initSql
                secret
                ;
```

- [ ] **Step 3: Replace the `owner` boolean option with the `role` enum**

In the `roles.client` interface, replace the whole `owner` option (lines 72-76):

```nix
                      owner = lib.mkOption {
                        type = lib.types.bool;
                        default = false;
                        description = "Whether this role owns the database (gets ALTER DATABASE ... OWNER TO).";
                      };
```

with:

```nix
                      role = lib.mkOption {
                        type = lib.types.enum [
                          "owner"
                          "readwrite"
                          "readonly"
                          "none"
                        ];
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

- [ ] **Step 4: Server `let` — redefine `ownersByDb`, add `ownerByDb` and `grantSql`**

In the server `perInstance` → `nixosModule` `let` block (currently lines 148-157, ending with `tmplName = e: ...`), replace the `ownersByDb` binding and add `ownerByDb` + `grantSql`. The existing `ownersByDb` line is:

```nix
                ownersByDb = lib.mapAttrs (_db: es: map (e: e.user) (lib.filter (e: e.owner) es)) (
                  lib.groupBy (e: e.database) entries
                );
```

Replace it (and extend the `let`) with:

```nix
                ownersByDb = lib.mapAttrs (_db: es: map (e: e.user) (lib.filter (e: e.role == "owner") es)) (
                  lib.groupBy (e: e.database) entries
                );
                # database -> its single owner user, for dbs that have one.
                ownerByDb = lib.mapAttrs (_db: lib.head) (
                  lib.filterAttrs (_db: owners: owners != [ ]) ownersByDb
                );
                # Privilege template SQL for one entry. Each non-empty block opens with
                # \connect so schema-local statements target the right database (the
                # provisioning psql runs without -d). ownerUser anchors ALTER DEFAULT
                # PRIVILEGES; guarded so a missing owner surfaces the assertion, not a
                # null-coercion error.
                grantSql =
                  e: ownerUser:
                  let
                    db = e.database;
                    u = e.user;
                    defaultPrivs =
                      grants:
                      lib.optionalString (ownerUser != null) ''
                        ALTER DEFAULT PRIVILEGES FOR ROLE "${ownerUser}" IN SCHEMA public GRANT ${grants};
                      '';
                  in
                  {
                    owner = ''
                      \connect "${db}"
                      ALTER SCHEMA public OWNER TO "${u}";
                    '';
                    readwrite = ''
                      \connect "${db}"
                      GRANT CONNECT ON DATABASE "${db}" TO "${u}";
                      GRANT USAGE ON SCHEMA public TO "${u}";
                      GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO "${u}";
                      GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO "${u}";
                      ${defaultPrivs ''SELECT, INSERT, UPDATE, DELETE ON TABLES TO "${u}"''}
                      ${defaultPrivs ''USAGE, SELECT ON SEQUENCES TO "${u}"''}
                    '';
                    readonly = ''
                      \connect "${db}"
                      GRANT CONNECT ON DATABASE "${db}" TO "${u}";
                      GRANT USAGE ON SCHEMA public TO "${u}";
                      GRANT SELECT ON ALL TABLES IN SCHEMA public TO "${u}";
                      ${defaultPrivs ''SELECT ON TABLES TO "${u}"''}
                    '';
                    none = "";
                  }
                  .${e.role};
```

- [ ] **Step 5: Template content — gate `ALTER DATABASE` on `role`, inject `grantSql`**

Replace the `content` block in `sops.templates` (lines 193-199):

```nix
                      content = ''
                        ALTER USER "${e.user}" WITH PASSWORD '${
                          config.sops.placeholder."${secretName instanceName e.database e.user}"
                        }';
                        ${lib.optionalString e.owner ''ALTER DATABASE "${e.database}" OWNER TO "${e.user}";''}
                        ${e.initSql}
                      '';
```

with:

```nix
                      content = ''
                        ALTER USER "${e.user}" WITH PASSWORD '${
                          config.sops.placeholder."${secretName instanceName e.database e.user}"
                        }';
                        ${lib.optionalString (
                          e.role == "owner"
                        ) ''ALTER DATABASE "${e.database}" OWNER TO "${e.user}";''}
                        ${grantSql e (ownerByDb.${e.database} or null)}
                        ${e.initSql}
                      '';
```

- [ ] **Step 6: Assertion — exactly one owner per database**

Replace the `assertions` block (lines 231-234):

```nix
                assertions = lib.mapAttrsToList (db: owners: {
                  assertion = lib.length owners <= 1;
                  message = "postgresql clan module: database '${db}' has multiple owners (${lib.concatStringsSep ", " owners}); at most one access entry per database may set owner = true.";
                }) ownersByDb;
```

with:

```nix
                assertions = map (db: {
                  assertion = lib.length (ownersByDb.${db} or [ ]) == 1;
                  message =
                    let
                      owners = ownersByDb.${db} or [ ];
                    in
                    "postgresql clan module: database '${db}' must have exactly one access entry with role = \"owner\" (found ${toString (lib.length owners)}${lib.optionalString (owners != [ ]) ": ${lib.concatStringsSep ", " owners}"}).";
                }) databases;
```

- [ ] **Step 7: Migrate metabase to the `role` enum**

In `modules/machines/metabase.nix`, the access entry (line 33-40) currently starts with `owner = true;`. Change that single line:

```nix
  clan.inventory.instances.postgres.roles.client.machines.metabase.settings.access.metabase = {
    role = "owner";
    sourceCIDRs = [
      "100.64.0.0/10"
      "192.168.2.36/32"
    ];
    restartUnits = [ "metabase.service" ];
  };
```

- [ ] **Step 8: Format**

Run: `nix fmt`
Expected: exits 0; `modules/clan/postgresql.nix` and `modules/machines/metabase.nix` reformatted if needed.

- [ ] **Step 9: Run the test — both configs eval and metabase now takes schema ownership**

Run:
```bash
nix eval --raw '.#nixosConfigurations.postgres.config.sops.templates."postgresql-provision-metabase-metabase.sql".content'
```
Expected: output now contains BOTH `ALTER DATABASE "metabase" OWNER TO "metabase";` AND `ALTER SCHEMA public OWNER TO "metabase";` (the owner block from `grantSql`).

Run (forces full eval incl. assertions for both hosts — the interface change must not break metabase):
```bash
nix eval '.#nixosConfigurations.postgres.config.system.build.toplevel'
nix eval '.#nixosConfigurations.metabase.config.system.build.toplevel'
```
Expected: both print a `/nix/store/...-nixos-system-...` path with no eval error.

- [ ] **Step 10: Commit**

```bash
git add modules/clan/postgresql.nix modules/machines/metabase.nix
git commit -m "feat(postgresql): role-based grant generation in clan module

Replace the per-entry owner boolean with a role enum
(owner/readwrite/readonly/none); the server renders matching GRANT and
ALTER DEFAULT PRIVILEGES SQL (each block \connect-scoped) into the
provisioning oneshots. Owners now also take ALTER SCHEMA public OWNER.
Assert exactly one owner per database. Migrate metabase to role = owner.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: Declare the hledger database in postgres.nix

**Files:**
- Modify: `modules/machines/postgres.nix` (the `clan.inventory.instances.postgres` block, lines 35-41)

**Interfaces:**
- Consumes: the `role` enum, `grantSql`, and exactly-one-owner assertion from Task 1.
- Produces: three `(hledger, *)` provisioning artifacts on the postgres host — roles `hledger` / `hledger_rw` / `hledger_ro`, database `hledger`, six `hostssl` pg_hba lines, three shared-password generators, three rendered provisioning templates.

- [ ] **Step 1: Failing test — the hledger provisioning template does not exist yet**

Run:
```bash
nix eval --raw '.#nixosConfigurations.postgres.config.sops.templates."postgresql-provision-hledger-hledger_ro.sql".content'
```
Expected: FAIL — eval error that attribute `"postgresql-provision-hledger-hledger_ro.sql"` is missing (no hledger entries declared yet).

- [ ] **Step 2: Add the three hledger access entries**

In `modules/machines/postgres.nix`, the instance block is currently:

```nix
  clan.inventory.instances.postgres = {
    module = {
      input = "self";
      name = "postgresql";
    };
    roles.server.machines.postgres = { };
  };
```

Replace it with (adds the postgres host as its own `client` and the three roles):

```nix
  clan.inventory.instances.postgres = {
    module = {
      input = "self";
      name = "postgresql";
    };
    roles.server.machines.postgres = { };
    roles.client.machines.postgres.settings.access =
      let
        tsCIDRs = [
          "100.64.0.0/10"
          "fd7a:115c:a1e0::/48"
        ];
      in
      {
        hledger = {
          database = "hledger";
          user = "hledger";
          role = "owner";
          sourceCIDRs = tsCIDRs;
        };
        hledger-rw = {
          database = "hledger";
          user = "hledger_rw";
          role = "readwrite";
          sourceCIDRs = tsCIDRs;
        };
        hledger-ro = {
          database = "hledger";
          user = "hledger_ro";
          role = "readonly";
          sourceCIDRs = tsCIDRs;
        };
      };
  };
```

- [ ] **Step 3: Format**

Run: `nix fmt`
Expected: exits 0.

- [ ] **Step 4: Verify — pg_hba lines**

Run:
```bash
nix eval --raw '.#nixosConfigurations.postgres.config.services.postgresql.authentication' | grep hledger
```
Expected: six lines, one per (user × CIDR):
```
hostssl hledger hledger 100.64.0.0/10 scram-sha-256
hostssl hledger hledger fd7a:115c:a1e0::/48 scram-sha-256
hostssl hledger hledger_rw 100.64.0.0/10 scram-sha-256
hostssl hledger hledger_rw fd7a:115c:a1e0::/48 scram-sha-256
hostssl hledger hledger_ro 100.64.0.0/10 scram-sha-256
hostssl hledger hledger_ro fd7a:115c:a1e0::/48 scram-sha-256
```

- [ ] **Step 5: Verify — database, roles, and shared-password generators exist**

Run:
```bash
nix eval '.#nixosConfigurations.postgres.config.clan.core.postgresql.databases.hledger.name'
nix eval '.#nixosConfigurations.postgres.config.clan.core.vars.generators."postgresql-postgres-hledger-hledger".share'
nix eval '.#nixosConfigurations.postgres.config.clan.core.vars.generators."postgresql-postgres-hledger-hledger_rw".share'
nix eval '.#nixosConfigurations.postgres.config.clan.core.vars.generators."postgresql-postgres-hledger-hledger_ro".share'
```
Expected: `"hledger"`, then `true`, `true`, `true`.

- [ ] **Step 6: Verify — rendered grant SQL per role**

Run:
```bash
nix eval --raw '.#nixosConfigurations.postgres.config.sops.templates."postgresql-provision-hledger-hledger.sql".content'
```
Expected: contains `ALTER DATABASE "hledger" OWNER TO "hledger";`, `\connect "hledger"`, and `ALTER SCHEMA public OWNER TO "hledger";`.

Run:
```bash
nix eval --raw '.#nixosConfigurations.postgres.config.sops.templates."postgresql-provision-hledger-hledger_rw.sql".content'
```
Expected: contains `\connect "hledger"`, `GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO "hledger_rw";`, `GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO "hledger_rw";`, and `ALTER DEFAULT PRIVILEGES FOR ROLE "hledger" IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO "hledger_rw";`.

Run:
```bash
nix eval --raw '.#nixosConfigurations.postgres.config.sops.templates."postgresql-provision-hledger-hledger_ro.sql".content'
```
Expected: contains `\connect "hledger"`, `GRANT SELECT ON ALL TABLES IN SCHEMA public TO "hledger_ro";`, and `ALTER DEFAULT PRIVILEGES FOR ROLE "hledger" IN SCHEMA public GRANT SELECT ON TABLES TO "hledger_ro";`. Does **not** contain `INSERT`.

- [ ] **Step 7: Whole-config sanity**

Run (forces full eval incl. the exactly-one-owner assertion — `hledger` and `metabase` each have exactly one owner):
```bash
nix eval '.#nixosConfigurations.postgres.config.system.build.toplevel'
```
Expected: prints a `/nix/store/...-nixos-system-...` path with no eval error.

- [ ] **Step 8: Commit**

```bash
git add modules/machines/postgres.nix
git commit -m "feat(postgres): add hledger database with owner/rw/ro roles

Declare the hledger database on the postgres host (acting as its own
clan client) with owner, read-write, and read-only roles, reachable
from the whole tailnet (IPv4 CGNAT + IPv6 ULA) over SSL.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Post-Implementation: Deploy & Manual Verification

Not part of the TDD tasks — run after both tasks are committed and the change is deployed to the `postgres` host.

1. Generate the new shared passwords: `clan vars generate postgres` (or the repo's equivalent), then deploy the `postgres` machine.
2. Read a role password on the postgres host to hand to clients:
   `sudo cat /run/secrets/vars/postgresql-postgres-hledger-hledger_ro/password`.
3. From a tailnet peer, verify each role's privileges:
   - `psql "postgres://postgres.lynx-lizard.ts.net/hledger?user=hledger_ro&sslmode=require"` → `SELECT` works, `INSERT` denied.
   - same as `hledger_rw` → `INSERT`/`UPDATE`/`DELETE` work.
   - same as `hledger` (owner) → `CREATE TABLE t (id int);`, then confirm `hledger_rw` can `INSERT` into `t` and `hledger_ro` can `SELECT` from `t` **without any further grant** (proves `ALTER DEFAULT PRIVILEGES` covers future tables).

---

## Notes & Known Limitations

- **Self-client password rotation:** because the postgres host is both `server` and `client` for the hledger entries, the two generated password generators per role collapse to one (client definition wins), dropping the generator→provisioning-oneshot `restartUnits` link. First-time provisioning is unaffected; on a password *rotation* a plain re-deploy re-runs the oneshot (`wantedBy = multi-user.target`). Accepted, not worked around. (Spec §"Known limitation".)
- **metabase behavior change:** the migration additionally runs `ALTER SCHEMA public OWNER TO "metabase"` — additive (grants, never revokes), idempotent, safe.
