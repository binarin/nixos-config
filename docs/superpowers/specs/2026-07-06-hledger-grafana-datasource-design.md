# hledger as a Grafana datasource on monitor

**Date:** 2026-07-06
**Status:** design approved, pending implementation

## Goal

Give the `monitor` machine's Grafana read-only access to the `hledger`
PostgreSQL database, using the existing `postgresql` clan service for
credential provisioning and distribution. Grafana connects as a dedicated
`hledger_monitor_ro` role over Tailscale.

## Context

- The `postgres` machine runs the `postgresql` clan service
  (`modules/clan/postgresql.nix`), which provisions roles/databases and
  distributes shared passwords (`share = true` clan-vars generators keyed by
  `(database, user)`).
- The `hledger` database and its `hledger` (owner) / `hledger_rw` / `hledger_ro`
  roles already exist. Journal → postgres ETL is handled **externally** and is
  out of scope here.
- `monitor` (`modules/machines/monitor.nix`) runs Grafana with a VictoriaMetrics
  datasource. Grafana already reads its admin/secret-key secrets via
  `$__file{${... clan var path}}`.
- `metabase` (`modules/machines/metabase.nix`) is the direct precedent for a
  clan postgres *consumer*: it declares a `roles.client` access entry and
  connects to `postgres.lynx-lizard.ts.net:5432 … sslmode=require`.
- Connectivity already works: other tailnet machines reach
  `postgres.lynx-lizard.ts.net:5432` today, so no firewall change is needed.

## Design

A dedicated read-only role `hledger_monitor_ro` is created specifically for
Grafana — its own credential, independently revocable, distinct from the
existing `hledger_ro`. Because the role is new and used only by monitor, **all
changes live in `monitor.nix`; `postgres.nix` is untouched.** The `postgresql`
service's `server` role flattens client access entries across every machine, so
declaring the entry on monitor is sufficient for the server to create the role,
apply the `readonly` grants, and emit the `hostssl` pg_hba line.

### 1. Client access entry (`modules/machines/monitor.nix`)

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

Notes:
- `database` and `user` are set explicitly — the access-entry label
  (`hledger-monitor-ro`) would otherwise default both to the label string.
- `role = "readonly"` applies the module's readonly grant template (CONNECT,
  USAGE, SELECT on all current + future tables in `public`).
- `secret.owner = "grafana"` makes the shared-password generator deploy the
  password file to monitor owned by `grafana` (default mode `0400`), so the
  grafana service can read it. `restartUnits` restarts grafana on rotation.

### 2. Grafana datasource (`modules/machines/monitor.nix`)

Append to `services.grafana.provision.datasources.settings.datasources`:

```nix
{
  name = "hledger";
  type = "postgres";                       # core datasource; no declarativePlugin
  url = "postgres.lynx-lizard.ts.net:5432";
  user = "hledger_monitor_ro";
  database = "hledger";
  jsonData = {
    sslmode = "require";
    postgresVersion = 1800;                # postgresql_18
  };
  secureJsonData.password =
    "$__file{${config.clan.core.vars.generators."postgresql-postgres-hledger-hledger_monitor_ro".files.password.path}}";
}
```

The provisioning file is written to the world-readable Nix store, so the
password is referenced by **path** via `$__file{}`, never inlined — mirroring
how `monitor.nix` already feeds Grafana its admin credentials. The clan-var
generator name follows the module's convention
`postgresql-<instance>-<database>-<user>`, i.e.
`postgresql-postgres-hledger-hledger_monitor_ro`; this generator is declared on
monitor by the service's `perMachine` (client) logic, so `.files.password.path`
resolves to the deployed secret path.

## Data flow

1. `clan vars generate` creates a shared `hledger_monitor_ro` password
   (generator `postgresql-postgres-hledger-hledger_monitor_ro`).
2. Deployed to **monitor** (file owned by `grafana`) and used on **postgres**
   (server sets it via `ALTER USER`, applies readonly grants, adds the
   `hostssl hledger hledger_monitor_ro <cidr> scram-sha-256` line).
3. Grafana reads the password file at runtime and connects to
   `postgres.lynx-lizard.ts.net:5432` over Tailscale, SSL-encrypted,
   authenticating as `hledger_monitor_ro`.

## Out of scope

- Journal → postgres ETL (handled externally).
- Firewall / network reachability (already working).
- Grafana dashboards — only the datasource is created here.

## Verification

- `nix` build/eval of both `monitor` and `postgres` configurations succeeds.
- `clan vars generate` produces the new generator; deploy both machines.
- On monitor: the `hledger_monitor_ro` password file exists, owned by `grafana`.
- On postgres: role `hledger_monitor_ro` exists with readonly privileges; the
  new `hostssl` pg_hba line is present.
- In Grafana: *Connections → hledger → Test* succeeds; a `SELECT` query returns
  data; an `INSERT`/`UPDATE` is denied (readonly confirmed).

## Risks

- **`$__file{}` inside `secureJsonData`** — documented as supported in Grafana
  provisioning since 7.1. If it does not expand as expected, fall back to
  rendering the password into a `grafana`-owned file via a `sops.template`
  (as `metabase.nix` does for its connection URI) and `$__file{}` that path.
