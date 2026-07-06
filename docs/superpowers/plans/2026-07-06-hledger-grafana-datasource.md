# hledger Grafana Datasource Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give `monitor`'s Grafana read-only access to the `hledger` PostgreSQL database via a dedicated `hledger_monitor_ro` role, provisioned and distributed through the `postgresql` clan service.

**Architecture:** All changes are declarative NixOS config in a single file, `modules/machines/monitor.nix`. Monitor is added as a `client` of the existing `postgres` clan instance with one readonly access entry; the clan service's `server` role (on the `postgres` machine) creates the role/grants/pg_hba automatically from that declaration. Grafana gets a core `postgres` provisioned datasource whose password is read from the deployed clan-var file via `$__file{}`.

**Tech Stack:** Nix / flake-parts (dendritic pattern), clan (`clan.core.vars`, `clan.inventory.instances`), NixOS `services.grafana`, deploy-rs, sops-nix.

## Global Constraints

- All edits are confined to `modules/machines/monitor.nix`. `modules/machines/postgres.nix` and `modules/clan/postgresql.nix` MUST NOT be modified.
- Connection host: `postgres.lynx-lizard.ts.net:5432`, `sslmode=require` (verbatim).
- Dedicated role name: `hledger_monitor_ro`; database: `hledger`; access-entry label: `hledger-monitor-ro`.
- Clan-vars generator name (derived by the module as `postgresql-<instance>-<database>-<user>`): `postgresql-postgres-hledger-hledger_monitor_ro`.
- Password is referenced by **path** via `$__file{}` — never inlined into the store.
- Secret generation (`clan vars generate`) and deploy are **user-run** (the assistant's sops identity cannot decrypt machine secrets). Tasks mark these explicitly.
- Grafana datasource `postgres` is a core datasource — do **not** add a `declarativePlugins` entry for it.

---

### Task 1: Add monitor as a postgres client + Grafana hledger datasource

**Files:**
- Modify: `modules/machines/monitor.nix` — add a top-level `clan.inventory.instances.postgres...` attr (alongside the existing `clan.inventory.machines.monitor` block, ~line 19), and append one datasource to `services.grafana.provision.datasources.settings.datasources` (currently a single-element list at ~lines 118-126).

**Interfaces:**
- Consumes: the existing `postgres` clan instance (defined in `modules/machines/postgres.nix` with `module = { input = "self"; name = "postgresql"; }`) and its `roles.client` access submodule (fields: `database`, `user`, `role`, `sourceCIDRs`, `restartUnits`, `secret.owner`).
- Consumes: `config.clan.core.vars.generators."postgresql-postgres-hledger-hledger_monitor_ro".files.password.path`, declared on monitor by the service's `perMachine` client logic once the access entry below exists.
- Produces: PostgreSQL role `hledger_monitor_ro` (readonly on `hledger`) and a Grafana datasource named `hledger`. No later task depends on new Nix symbols.

- [ ] **Step 1: Add the client access entry**

Insert this as a new top-level attribute inside the outer attrset of `modules/machines/monitor.nix` (place it right after the `clan.inventory.machines.monitor = { ... };` block, ~line 21). It uses the flake-level `config` arg already in scope (the file's header is `{ self, config, lib, ... }` with `flakeConfig = config`):

```nix
  clan.inventory.instances.postgres.roles.client.machines.monitor.settings.access.hledger-monitor-ro = {
    database = "hledger";
    user = "hledger_monitor_ro";
    role = "readonly";
    sourceCIDRs = [
      "100.64.0.0/10"
      "fd7a:115c:a1e0::/48"
    ];
    restartUnits = [ "grafana.service" ];
    secret.owner = "grafana";
  };
```

- [ ] **Step 2: Add the Grafana datasource**

In the `flake.nixosModules.monitor-configuration` module, the `provision.datasources.settings.datasources` list currently holds one VictoriaMetrics entry (~lines 118-126). Add a second element so the list reads:

```nix
            datasources = [
              {
                name = "VictoriaMetrics";
                type = "victoriametrics-metrics-datasource";
                access = "proxy";
                url = "http://127.0.0.1:8428";
                isDefault = true;
              }
              {
                name = "hledger";
                type = "postgres";
                access = "proxy";
                url = "postgres.lynx-lizard.ts.net:5432";
                user = "hledger_monitor_ro";
                database = "hledger";
                jsonData = {
                  sslmode = "require";
                  postgresVersion = 1800;
                };
                secureJsonData.password =
                  "$__file{${config.clan.core.vars.generators."postgresql-postgres-hledger-hledger_monitor_ro".files.password.path}}";
              }
            ];
```

Note: inside `monitor-configuration` the module argument is also named `config` (the NixOS config), which is the correct scope for `config.clan.core.vars.generators...` — this is the same `config` already used two lines up for the grafana admin secrets (`config.clan.core.vars.generators.grafana.files.*.path`).

- [ ] **Step 3: Lint**

Run: `nix fmt modules/machines/monitor.nix`
Expected: file reformatted (or unchanged), no errors.

- [ ] **Step 4: Evaluate the monitor toplevel (proves the generator path resolves)**

Run: `nix build --no-link '.#nixosConfigurations.monitor.config.system.build.toplevel' 2>&1 | tail -20`
Expected: builds/evaluates with no error. A failure mentioning the attribute path `postgresql-postgres-hledger-hledger_monitor_ro` means the access entry (Step 1) is missing or mis-typed — the datasource references a generator that only exists once monitor is a client.

- [ ] **Step 5: Evaluate the postgres toplevel (proves server-side provisioning is well-formed)**

Run: `nix build --no-link '.#nixosConfigurations.postgres.config.system.build.toplevel' 2>&1 | tail -20`
Expected: builds/evaluates with no error. The server role flattens monitor's access entry into a provisioning unit + pg_hba line + shared generator; an assertion failure here would name the `hledger` database (e.g. owner-count), which this change must not trigger.

- [ ] **Step 6: Confirm the datasource password is not inlined into the store**

Run: `nix eval --raw '.#nixosConfigurations.monitor.config.services.grafana.provision.datasources.settings' --apply builtins.toJSON 2>/dev/null | grep -o '\$__file{[^}]*hledger_monitor_ro[^}]*}'`
Expected: prints a single `$__file{/…/postgresql-postgres-hledger-hledger_monitor_ro/…}` path (the literal `$__file{}` wrapper, not a hex secret). If it prints a 64-char hex string instead, the password leaked — revert to `$__file{}`.

- [ ] **Step 7: Commit**

```bash
git add modules/machines/monitor.nix docs/superpowers/plans/2026-07-06-hledger-grafana-datasource.md
git commit -m "feat(monitor): grafana hledger_monitor_ro readonly datasource

Add monitor as a postgresql clan client with a dedicated
hledger_monitor_ro readonly role, and a core postgres Grafana
datasource reading its password via \$__file{}. postgres.nix is
untouched; the server role provisions the role/grants/pg_hba from
monitor's access declaration.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: Generate secret, deploy, and verify (USER-RUN)

**Files:** none (operational). This task is run by the user; the assistant should hand off and wait, then help interpret output.

**Interfaces:**
- Consumes: the committed config from Task 1.
- Produces: a deployed `hledger_monitor_ro` password on both machines and a working Grafana datasource.

- [ ] **Step 1: Generate the shared password (user)**

Run: `clan vars generate postgres` (and, if not shared automatically, `clan vars generate monitor`)
Expected: a new generator `postgresql-postgres-hledger-hledger_monitor_ro` appears; its `password` file is created and encrypted for both `postgres` and `monitor`.

- [ ] **Step 2: Commit the generated vars (user)**

```bash
git add vars sops
git commit -m "chore(vars): hledger_monitor_ro shared password"
```
Expected: new files under `vars/` (and per-machine sops) are committed.

- [ ] **Step 3: Deploy postgres, then monitor (user)**

Run (postgres first so the role/pg_hba exist before Grafana connects):
`deploy .#postgres` then `deploy .#monitor`
Expected: both deploys succeed; `grafana.service` restarts on monitor (declared `restartUnits`).

- [ ] **Step 4: Verify role + password file**

On monitor: `sudo stat -c '%U %a' $(readlink -f /run/secrets/vars/postgresql-postgres-hledger-hledger_monitor_ro/password 2>/dev/null || echo /nonexistent)`
Expected: owner `grafana`, mode `400`. (Path may differ by clan version; the intent is: file exists, owned by grafana.)

On postgres: `sudo -u postgres psql -d hledger -c "\du hledger_monitor_ro"`
Expected: role listed. Then `sudo -u postgres psql -d hledger -c "\dp" | head` shows SELECT grants; the role has no INSERT/UPDATE/DELETE.

- [ ] **Step 5: Verify Grafana connectivity**

In the Grafana UI (monitor): *Connections → Data sources → hledger → Test*.
Expected: "Database Connection OK".

Then run an ad-hoc query (Explore → hledger): a `SELECT` against any hledger table returns rows; an attempted `INSERT`/`UPDATE` fails with a permission error (confirms readonly).

- [ ] **Step 6: If Test fails with a `$__file`/password error**

Fallback (per spec risk): render the password into a grafana-owned file via a `sops.template` (as `modules/machines/metabase.nix` does for its connection URI) and point `secureJsonData.password` at `$__file{${config.sops.templates."…".path}}`. Re-run Task 1 Steps 3-7, then redeploy monitor.

---

## Self-Review

**1. Spec coverage:**
- Client access entry (spec §1) → Task 1 Step 1. ✓
- Grafana datasource (spec §2) → Task 1 Step 2. ✓
- postgres.nix untouched → Global Constraints + no task modifies it. ✓
- Data flow (generate → deploy → connect) → Task 2. ✓
- Out of scope (ETL, firewall, dashboards) → not planned, correct. ✓
- Verification list (build, role, file owner, Grafana test, readonly) → Task 1 Steps 4-6 + Task 2 Steps 4-5. ✓
- Risk ($__file in secureJsonData) → Task 2 Step 6 fallback. ✓

**2. Placeholder scan:** No TBD/TODO; all code blocks are complete and literal. ✓

**3. Type/name consistency:** `hledger_monitor_ro` (user), `hledger-monitor-ro` (label), `hledger` (db), and generator `postgresql-postgres-hledger-hledger_monitor_ro` are identical across Global Constraints, Task 1, and Task 2. ✓
