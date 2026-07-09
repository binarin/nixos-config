# atuin → standalone PostgreSQL — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move `media`'s atuin sync server off media's local PostgreSQL 15 onto the standalone `postgres` machine (PostgreSQL 18, clan `postgresql` service), preserving synced history via a user-run dump/restore.

**Architecture:** Add `media` as a client of the existing `postgres` clan `postgresql` instance (one `access.atuin` entry, role `owner`) — this provisions an empty, atuin-owned DB + role + shared password server-side, with no changes to the generic clan module. On media, compose the connection string from the clan-shared password into a secret env-file (the existing `tandoor-env` pattern) and point `services.atuin` at it, disabling local DB creation. Cutover happens with atuin **masked** so a deploy can't start it against the empty DB before the restore.

**Tech Stack:** NixOS (dendritic/flake-parts modules), clan-core (`postgresql` clan service + vars generators), deploy-rs, PostgreSQL 15→18 dump/restore.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-09-atuin-standalone-postgres-design.md`. **atuin-only**; immich is a separate phase-2 spec — do not touch immich here.
- Password must **never** enter `/nix/store` — only via a `secret = true` clan-vars generator file.
- Connection address: `postgres.lynx-lizard.ts.net:5432`, `sslmode=require` (matches `metabase`/`monitor` precedent and the tailscale `sourceCIDRs`).
- tailscale `sourceCIDRs` verbatim: `[ "100.64.0.0/10" "fd7a:115c:a1e0::/48" ]`.
- Shared password generator name (produced by the clan module): `postgresql-postgres-atuin-atuin`, file `password`.
- All shell / `clan vars generate` / `deploy` operations are **user-run** (agent's sops identity cannot decrypt machine secrets). The agent supplies exact commands; it does not run deploys or secret generation.
- Deploy tool: deploy-rs, from the repo root inside `nix develop` — `deploy .#postgres`, `deploy .#media`.
- Lint before commit: `nix fmt` on changed files (repo uses nixfmt via `just lint`).

---

### Task 1: Server — provision the atuin DB/role on the standalone postgres

Add `media` as a client of the `postgres` clan `postgresql` instance. This is self-contained and harmless on its own (it only creates an empty database, role, password, pg_hba lines).

**Files:**
- Modify: `modules/machines/postgres.nix` (inside `clan.inventory.instances.postgres.roles.client`, alongside the existing `machines.postgres.settings.access` block, ~lines 41–67)

**Interfaces:**
- Consumes: nothing (first task).
- Produces (relied on by Task 2 and the cutover):
  - clan-shared password generator `postgresql-postgres-atuin-atuin`, file `password` (raw hex, no trailing newline), `share = true`, decryptable by both `postgres` and `media`.
  - On `postgres`: database `atuin`, role `atuin` (owns DB + `public` schema), `hostssl atuin atuin <cidr> scram-sha-256` pg_hba lines, systemd unit `postgresql-provision-atuin-atuin.service`.

- [ ] **Step 1: Add the media client access entry**

In `modules/machines/postgres.nix`, locate the `clan.inventory.instances.postgres` block. It currently ends with the `roles.client.machines.postgres.settings.access = let … in { hledger…; };` block. Add a sibling `roles.client.machines.media` entry immediately after the closing `;` of that block (still inside `clan.inventory.instances.postgres = { … };`):

```nix
    roles.client.machines.media.settings.access = {
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

- [ ] **Step 2: Format the file**

Run: `nix fmt modules/machines/postgres.nix`
Expected: reformats/no-ops cleanly, exit 0.

- [ ] **Step 3: Verify the postgres config evaluates and provisions atuin**

Run:
```bash
nix eval --json '.#nixosConfigurations.postgres.config.clan.core.postgresql.databases' --apply 'builtins.attrNames'
```
Expected: a JSON array that includes `"atuin"` (alongside `"hledger"`).

Run:
```bash
nix eval --raw '.#nixosConfigurations.postgres.config.systemd.services."postgresql-provision-atuin-atuin".description'
```
Expected: `Provision PostgreSQL role atuin / database atuin`

- [ ] **Step 4: Verify the shared password generator exists on both machines**

Run:
```bash
nix eval --raw '.#nixosConfigurations.postgres.config.clan.core.vars.generators."postgresql-postgres-atuin-atuin".files.password.path'
nix eval --raw '.#nixosConfigurations.media.config.clan.core.vars.generators."postgresql-postgres-atuin-atuin".files.password.path'
```
Expected: both print a `/run/secrets/…` (or clan vars) path without error. (An eval error here means media is not yet seen as a client — recheck Step 1.)

- [ ] **Step 5: Commit**

```bash
git add modules/machines/postgres.nix
git commit -m "postgres: provision atuin database/role for media client"
```

---

### Task 2: Client — wire media's atuin to the standalone DB, masked (prepared state)

Compose the connection env-file from the shared password, point atuin at it, stop local DB creation, and **mask** the atuin unit so the upcoming deploy cannot start it against the empty DB. Together with Task 1 this is the spec's "Commit A / prepared state".

**Files:**
- Modify: `modules/machines/media.nix` (the `services.atuin.enable = true;` line is at ~801; add the generator + settings + mask near it, inside the `config = { … }` block)

**Interfaces:**
- Consumes (from Task 1): clan-shared generator `postgresql-postgres-atuin-atuin`, file `password`.
- Produces: media-local generator `atuin-db-env`, file `env` (contents: a single line `ATUIN_DB_URI=postgresql://atuin:<pw>@postgres.lynx-lizard.ts.net:5432/atuin?sslmode=require`); `services.atuin` configured for the remote DB; `systemd.services.atuin` masked.

- [ ] **Step 1: Replace the atuin config with the remote-DB + masked version**

In `modules/machines/media.nix`, find:

```nix
        services.atuin.enable = true;
```

Replace that single line with:

```nix
        # atuin history DB lives on the standalone `postgres` machine (clan
        # postgresql instance). The password is composed into an ATUIN_DB_URI
        # env-file (tandoor-env pattern) so it never enters /nix/store.
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

        services.atuin = {
          enable = true;
          database.createLocally = false;
          database.uri = lib.mkForce null;
          environmentFile = config.clan.core.vars.generators.atuin-db-env.files.env.path;
        };

        # Prepared-state mask: keep atuin from starting against the empty remote
        # DB (and auto-running sqlx migrations) before the manual restore.
        # Removed in the cutover task (Task 5).
        systemd.services.atuin.enable = false;
```

Note: `lib` and `config` are already in scope in this module (the `media-configuration` module header is `{ config, lib, pkgs, ... }:`).

- [ ] **Step 2: Format the file**

Run: `nix fmt modules/machines/media.nix`
Expected: exit 0.

- [ ] **Step 3: Verify media evaluates: env generator present, local DB off, unit masked**

Run:
```bash
nix eval --raw '.#nixosConfigurations.media.config.clan.core.vars.generators.atuin-db-env.script'
```
Expected: prints the script containing `ATUIN_DB_URI=postgresql://atuin:%s@postgres.lynx-lizard.ts.net:5432/atuin?sslmode=require`.

Run:
```bash
nix eval '.#nixosConfigurations.media.config.services.atuin.database.createLocally'
nix eval '.#nixosConfigurations.media.config.systemd.services.atuin.enable'
```
Expected: `false` and `false` respectively.

- [ ] **Step 4: Verify the full media system closure still builds**

Run:
```bash
nix build --no-link '.#nixosConfigurations.media.config.system.build.toplevel'
```
Expected: builds successfully (exit 0). This catches any `mkForce`/scope errors and confirms the masked unit + immich's still-local postgres coexist.

- [ ] **Step 5: Commit (this is "Commit A" — prepared state)**

```bash
git add modules/machines/media.nix
git commit -m "media: point atuin at standalone postgres (masked, prepared state)"
```

---

### Task 3: Deploy the prepared state (USER-RUN)

Generate the shared secret + env-file and deploy both machines. After this, `postgres` has an empty atuin DB and `media` has atuin masked (not running); the existing pg15 atuin DB on media is untouched.

**Files:** none (operational).

**Interfaces:**
- Consumes: committed configs from Tasks 1–2.
- Produces: live empty `atuin` DB/role on `postgres`; masked atuin on `media`.

- [ ] **Step 1: Generate vars (creates the shared password + composed env-file, auto-commits)**

Run (from repo root, inside `nix develop`):
```bash
clan vars generate postgres
clan vars generate media
```
Expected: clan reports generating `postgresql-postgres-atuin-atuin` (once; the second machine sees it as shared) and `atuin-db-env`, and creates `vars: update …` commit(s).

- [ ] **Step 2: Deploy postgres first (provisions the empty DB before media needs it)**

Run: `deploy .#postgres`
Expected: activation succeeds.

- [ ] **Step 3: Verify the empty atuin DB/role exists and provisioning succeeded**

Run:
```bash
ssh root@postgres.lynx-lizard.ts.net 'systemctl is-active postgresql-provision-atuin-atuin.service'
ssh root@postgres.lynx-lizard.ts.net "sudo -u postgres psql -tAc \"select datname from pg_database where datname='atuin'\""
ssh root@postgres.lynx-lizard.ts.net "sudo -u postgres psql -d atuin -tAc '\dt'"
```
Expected: `active`; prints `atuin`; the `\dt` prints `Did not find any relations.` (empty DB).

- [ ] **Step 4: Deploy media (atuin comes up masked, i.e. NOT running)**

Run: `deploy .#media`
Expected: activation succeeds.

- [ ] **Step 5: Verify atuin is masked and the old pg15 DB is still present**

Run:
```bash
ssh root@media.lynx-lizard.ts.net 'systemctl is-enabled atuin.service || true'
ssh root@media.lynx-lizard.ts.net 'systemctl is-active atuin.service || true'
# NB: modern atuin (sync v2) stores data in the `store` record table, NOT `history`
# (which is legacy/empty). Parity must be measured on `store` (+ `records`).
ssh root@media.lynx-lizard.ts.net "sudo -u postgres psql -d atuin -tAc 'select count(*) from store'"
ssh root@media.lynx-lizard.ts.net "sudo -u postgres psql -d atuin -tAc 'select count(*) from records'"
```
Expected: `masked`; `inactive` (or `failed`/not running — must NOT be `active`); the `store` count is the real synced-data row count from the **old pg15** DB. **Record `store` (and `records`)** — call the `store` count `N_media`.

---

### Task 4: Dump and restore the atuin data (USER-RUN)

Move the history from media's pg15 into the standalone pg18 `atuin` DB while atuin is quiescent (masked).

**Files:** none (operational).

**Interfaces:**
- Consumes: empty pg18 `atuin` DB (Task 3), masked atuin (Task 3), `N_media` (Task 3 Step 5).
- Produces: pg18 `atuin` DB populated with `N_media` history rows.

- [ ] **Step 1: Dump from media's pg15 (custom format)**

Run:
```bash
ssh root@media.lynx-lizard.ts.net 'sudo -u postgres pg_dump -Fc -d atuin -f /tmp/atuin.dump'
```
Expected: exit 0; `/tmp/atuin.dump` created.

- [ ] **Step 2: Transfer the dump to the postgres box**

Run:
```bash
ssh root@media.lynx-lizard.ts.net 'cat /tmp/atuin.dump' | ssh root@postgres.lynx-lizard.ts.net 'cat > /tmp/atuin.dump'
```
Expected: exit 0. (Or use whatever transfer you prefer; the file must land at `/tmp/atuin.dump` on postgres.)

- [ ] **Step 3: Restore into the pg18 atuin DB (objects owned by role atuin)**

Run:
```bash
ssh root@postgres.lynx-lizard.ts.net 'sudo -u postgres pg_restore --no-owner --no-privileges --role=atuin -d atuin /tmp/atuin.dump'
```
Expected: exit 0 (or benign warnings only). `--role=atuin` makes restored objects atuin-owned; `--no-owner`/`--no-privileges` skip the dump's ownership/GRANT statements since the clan module already made `atuin` own the DB + `public` schema.

- [ ] **Step 4: Verify row parity**

Run:
```bash
ssh root@postgres.lynx-lizard.ts.net "sudo -u postgres psql -d atuin -tAc 'select count(*) from store'"
ssh root@postgres.lynx-lizard.ts.net "sudo -u postgres psql -d atuin -tAc 'select count(*) from records'"
```
Expected: the `store` count is **equal to `N_media`** (and `records` matches too) from Task 3 Step 5. If it differs, stop and investigate before unmasking (do NOT proceed to Task 5).

- [ ] **Step 5: Clean up the transferred dump (optional)**

Run:
```bash
ssh root@media.lynx-lizard.ts.net 'rm -f /tmp/atuin.dump'
ssh root@postgres.lynx-lizard.ts.net 'rm -f /tmp/atuin.dump'
```
Expected: exit 0.

---

### Task 5: Cutover — unmask atuin and deploy (Commit B)

Remove the mask so atuin runs against the standalone DB.

**Files:**
- Modify: `modules/machines/media.nix` (remove the `systemd.services.atuin.enable = false;` line added in Task 2)

**Interfaces:**
- Consumes: populated pg18 `atuin` DB (Task 4).
- Produces: live atuin serving from the standalone postgres.

- [ ] **Step 1: Remove the mask line**

In `modules/machines/media.nix`, delete the block:

```nix
        # Prepared-state mask: keep atuin from starting against the empty remote
        # DB (and auto-running sqlx migrations) before the manual restore.
        # Removed in the cutover task (Task 5).
        systemd.services.atuin.enable = false;
```

- [ ] **Step 2: Verify media evaluates with atuin unmasked**

Run:
```bash
nix eval '.#nixosConfigurations.media.config.systemd.services.atuin.enable'
```
Expected: `true`.

- [ ] **Step 3: Format and commit (this is "Commit B")**

```bash
nix fmt modules/machines/media.nix
git add modules/machines/media.nix
git commit -m "media: unmask atuin — cut over to standalone postgres"
```

- [ ] **Step 4: Deploy media (USER-RUN)**

Run: `deploy .#media`
Expected: activation succeeds.

- [ ] **Step 5: Verify atuin is live against the remote DB (USER-RUN)**

Run:
```bash
ssh root@media.lynx-lizard.ts.net 'systemctl is-active atuin.service'
ssh root@media.lynx-lizard.ts.net 'journalctl -u atuin.service -n 50 --no-pager'
```
Expected: `active`; logs show migrations clean and no connection/auth errors (no restart loop). Then from a machine with the atuin client configured:
```bash
atuin sync
```
Expected: sync succeeds against `atuin.binarin.info`.

---

### Task 6: Post-cutover cleanup and record the pattern

**Files:**
- Update (agent memory): `MEMORY.md` + a new memory file.

**Interfaces:**
- Consumes: verified working cutover (Task 5).

- [ ] **Step 1: Drop the stale pg15 atuin DB/role on media (optional, USER-RUN)**

Only after you're satisfied atuin works from the standalone DB. These linger harmlessly (no longer nix-managed) and will vanish when immich migrates and media's local postgres is removed in phase 2, so this step is optional:
```bash
ssh root@media.lynx-lizard.ts.net "sudo -u postgres psql -c 'DROP DATABASE atuin'"
ssh root@media.lynx-lizard.ts.net "sudo -u postgres psql -c 'DROP ROLE atuin'"
```
Expected: `DROP DATABASE` / `DROP ROLE`.

- [ ] **Step 2: Record the reusable pattern for the immich phase (agent memory)**

Write a `reference`/`project` memory capturing: "media is a client of the `postgres` clan `postgresql` instance; add DB access via an `access.<label>` entry in `postgres.nix` + a `tandoor-env`-style env-file generator in `media.nix`; connect via `postgres.lynx-lizard.ts.net:5432 sslmode=require`." Link it to the immich phase-2 spec appendix. Add a one-line pointer to `MEMORY.md`.

- [ ] **Step 3: Rollback note (reference only — do not run unless cutover failed)**

If atuin failed against the standalone DB at any point before you were satisfied: revert Commit A (and Commit B if made), `clan vars generate media`, `deploy .#media`. atuin returns to the untouched local pg15 DB.

---

## Self-Review

**Spec coverage:**
- §1 Server access entry → Task 1. ✅
- §2a membership → Task 1 (media listed as client) + verified Task 1 Step 4. ✅
- §2b env-file generator → Task 2 Step 1. ✅
- §2c createLocally/uri/environmentFile → Task 2 Step 1, verified Step 3. ✅
- §3 data flow → validated at runtime in Task 5 Step 5 (`atuin sync`). ✅
- §4 Commit A (server + client + mask) → Tasks 1–3. ✅
- §4 Dump → Task 4 Steps 1–2. ✅
- §4 Restore → Task 4 Step 3, parity check Step 4. ✅
- §4 Commit B (unmask) → Task 5. ✅
- §4 Cleanup → Task 6 Step 1. ✅
- §4 Rollback → Task 6 Step 3. ✅
- Risks/open points (dependency resolution, role-name collision, sslmode) → exercised by Task 1 Step 4 (dependency generator visible on media), Task 4 Step 3 (`--role=atuin`), and the `sslmode=require` in the Task 2 script. ✅

**Placeholder scan:** No TBD/TODO; `N_media` is an explicitly-captured runtime value, not a placeholder. Every code/command step shows exact content. ✅

**Type/name consistency:** Generator names (`postgresql-postgres-atuin-atuin`, `atuin-db-env`), file keys (`password`, `env`), unit name (`postgresql-provision-atuin-atuin.service`), DB/role (`atuin`), and the connection string are identical across Tasks 1, 2, 3, and 4. ✅
