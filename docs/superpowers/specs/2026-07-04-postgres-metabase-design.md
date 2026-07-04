# PostgreSQL + Metabase CT Design

**Date:** 2026-07-04
**Status:** Design approved

## 1. Overview

Two Proxmox LXC CTs: `postgres` (PostgreSQL 18) and `metabase` (Metabase web application).
Both are Tailscale nodes on the home network. Metabase uses PostgreSQL as its application
database. Database credentials are generated via clan vars (share=true) — never embedded in
the nix store.

## 2. Machine Resources

| Machine | IP | Cores | RAM | Rootfs | Mounts |
|---------|-----|-------|-----|--------|--------|
| `postgres` | 192.168.2.35 | 4 | 8192 MB | 32G | `/nix` (32G, nobackup), `/var/lib/postgresql` (separate ZFS vol) |
| `metabase` | 192.168.2.36 | 4 | 4096 MB | 32G | `/nix` (32G, nobackup) |

MAC addresses and IP allocations are already in `inventory/networks/home.toml`.

## 3. PostgreSQL Configuration (`postgres` machine)

### 3.1 Service

- **Package:** `postgresql_18`
- **Tuning for 8GB RAM:**
  - `shared_buffers = "2GB"`
  - `effective_cache_size = "6GB"`
  - `wal_level = "replica"` (clan default)
  - `max_wal_senders = 3` (clan default)

### 3.2 Database & User

Defined via `clan.core.postgresql`:

```nix
clan.core.postgresql.enable = true;
clan.core.postgresql.users.metabase = {};
clan.core.postgresql.databases.metabase = {};
```

This creates the `metabase` user (initially without password) and `metabase` database
via PostgreSQL's `postStart` script (embedded in `postgresql.service`).

### 3.3 Password Provisioning

A shared clan vars generator creates and shares the database password:

```nix
clan.core.vars.generators.metabase-db = {
  share = true;
  files.password = {
    secret = true;
    deploy = true;
  };
  script = ''
    tr -dc 'A-Za-z0-9' < /dev/urandom | head -c 32 > $out/password
  '';
};
```

A oneshot systemd service on `postgres` sets the password at runtime
(after `CREATE USER` has completed in `postStart`):

```
Unit: metabase-db-password.service
Requires=postgresql.service
After=postgresql.service

ExecStart: psql -c "ALTER USER metabase WITH PASSWORD '$(cat /run/secrets/metabase-db/password)'"
```

This keeps the password out of the nix store entirely — it exists only in the encrypted
clan vars store (at rest) and in `/run/secrets/` (tmpfs, at runtime).

### 3.4 Client Access (pg_hba.conf)

```
# Type  Database  User      Address            Method
host    metabase  metabase  192.168.2.36/32    md5
host    metabase  metabase  100.64.0.0/10      md5   # Tailscale subnet
```

No SSL required at the PostgreSQL protocol level. WireGuard (Tailscale) provides
L3 encryption on the Tailscale path. The direct home-net path is on a trusted subnet.

## 4. Metabase Configuration (`metabase` machine)

### 4.1 NixOS Module

Uses the upstream `services.metabase` module. Database connection via environment
variables pointing at PostgreSQL over the Tailscale network.

`EnvironmentFile` expects `KEY=VALUE` format but the clan var file contains only the
raw password, so a wrapper script reads the secret at runtime before exec'ing the JAR:

```nix
services.metabase = {
  enable = true;
  listen.port = 3000;
};

systemd.services.metabase = {
  environment = {
    MB_DB_TYPE = "postgres";
    MB_DB_HOST = "postgres.<ts-net>";  # Tailscale MagicDNS hostname
    MB_DB_PORT = "5432";
    MB_DB_DBNAME = "metabase";
    MB_DB_USER = "metabase";
  };
  serviceConfig.ExecStart = lib.mkForce (
    pkgs.writeShellScript "metabase-wrapper" ''
      export MB_DB_PASS="$(cat ${config.clan.core.vars.generators.metabase-db.files.password.path})"
      exec ${lib.getExe config.services.metabase.package}
    ''
  );
};
```

The `.path` reference resolves to `/run/secrets/metabase-db/password` at runtime
(tmpfs, never touches nix store).

### 4.2 Metabase Application Data

- Application database, user accounts, dashboards, settings: all stored in PostgreSQL
- `/var/lib/metabase` holds only plugins (`MB_PLUGINS_DIR`) and an unused H2 fallback
  (`MB_DB_FILE`) — all transient/re-downloadable. No separate mount needed.

## 5. Web Exposure (`metabase` machine)

### 5.1 Tailscale Serve (primary)

```nix
services.tailscale = {
  enable = true;
  serve = {
    services.metabase = {
      protocol = "https";
      target = "localhost:3000";
    };
  };
};
```

Provides automatic TLS via Tailscale. Accessible at `https://metabase.<ts-net>.ts.net`.

### 5.2 nginx + ACME (DNS-based)

```nix
services.nginx = {
  enable = true;
  virtualHosts."metabase.home.binarin.info" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://localhost:3000";
      recommendedProxySettings = true;
    };
  };
  virtualHosts."metabase.clan.binarin.info" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://localhost:3000";
      recommendedProxySettings = true;
    };
  };
};
```

ACME certificates provisioned via the clan `acme` module for both domains.
Both vhosts proxy to the same Metabase instance on localhost:3000.

## 6. Files to Create/Modify

### New files
None — machine modules already exist.

### Modified files
- `modules/machines/postgres.nix` — add Postgres service + CT resources + clan vars generator + oneshot
- `modules/machines/metabase.nix` — add Metabase service + CT resources + nginx + clan vars consumer

## 7. Testing

- `nix eval .#nixosConfigurations.postgres.config.system.build.toplevel` — postgres CT evals
- `nix eval .#nixosConfigurations.metabase.config.system.build.toplevel` — metabase CT evals
- `clan vars generate postgres --generator metabase-db --no-regenerate` — generate DB password
- Verify no password/secrets appear in nix store paths (check with `nix why-depends` or inspection)
