# PostgreSQL + Metabase CT Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Configure two Proxmox LXC CT machines — `postgres` (PostgreSQL 18) and `metabase` (Metabase web app) — with shared database credentials via clan vars (never in nix store), ACME certs for both, and Tailscale + nginx web exposure.

**Architecture:** Two independent NixOS CTs sharing a `metabase-db` clan vars generator (share=true). The generator creates the DB password on postgres; a oneshot service sets it at runtime via `ALTER USER`. Metabase consumes it via `LoadCredential` + `ImportCredential`. Certs via the clan `acme` module — postgres gets certs for native SSL, metabase gets certs for nginx vhosts.

**Tech Stack:** NixOS with clan-core vars, upstream `services.postgresql` (18.4) and `services.metabase` (0.59.2), Tailscale, nginx, clan ACME module (Cloudflare DNS-01)

## Global Constraints

- Passwords/secrets must never touch the nix store — use `clan.core.vars.generators` with `.path` references
- All connections from metabase to postgres go over Tailscale (WireGuard encrypted)
- Home-net pg_hba uses `hostssl` (PostgreSQL native SSL with ACME certs)
- Both machines are Proxmox LXC CTs, each with `/nix` separate mount
- Files: modify `modules/machines/postgres.nix` and `modules/machines/metabase.nix` only

---

## File Structure

| File | Responsibility |
|------|---------------|
| `modules/machines/postgres.nix` | Machine definition (clan inventory, CT resources, ACME client), NixOS module (PostgreSQL 18, clan.core.postgresql, vars generator, oneshot password-setter, pg_hba, SSL with combined PEM) |
| `modules/machines/metabase.nix` | Machine definition (clan inventory, CT resources, ACME client), NixOS module (Metabase service with LoadCredential, nginx vhosts, Tailscale Serve) |

---

### Task 1: Postgres CT resources

**Files:**
- Modify: `modules/machines/postgres.nix` — inside `flake.nixosModules.postgres-configuration`

**Interfaces:**
- Produces: `proxmoxLXC` block with cores, memory, mounts for postgres config

- [ ] **Step 1: Add proxmoxLXC configuration block**

In `flake.nixosModules.postgres-configuration`, after the `imports` list, add:

```nix
proxmoxLXC = {
  cores = 4;
  memory = 8192;
  mounts = [
    {
      mountPoint = "/nix";
      size = "32G";
      backup = false;
    }
    {
      mountPoint = "/var/lib/postgresql";
      size = "128G";
      backup = true;
    }
  ];
};
```

- [ ] **Step 2: Format**

```bash
nix fmt modules/machines/postgres.nix
```

- [ ] **Step 3: Verify eval**

Run:
```bash
nix eval .#nixosConfigurations.postgres.config.proxmoxLXC.cores
```
Expected: `4`

- [ ] **Step 4: Commit**

```bash
git add modules/machines/postgres.nix
git commit -m "feat(postgres): configure CT resources — 4 cores, 8GB RAM, mounts"
```

---

### Task 2: PostgreSQL 18 service with tuning

**Files:**
- Modify: `modules/machines/postgres.nix` — inside `flake.nixosModules.postgres-configuration`

**Interfaces:**
- Produces: `services.postgresql` enabled with PostgreSQL 18 and memory tuning
- Consumes: CT resources from Task 1 (no NixOS dependency — both are same module)

- [ ] **Step 1: Enable PostgreSQL with tuning**

In the same `postgres-configuration` module, add a `config` block after the `proxmoxLXC` block:

```nix
services.postgresql = {
  enable = true;
  package = pkgs.postgresql_18;
  settings = {
    shared_buffers = "2GB";
    effective_cache_size = "6GB";
  };
};
```

- [ ] **Step 2: Format**

```bash
nix fmt modules/machines/postgres.nix
```

- [ ] **Step 3: Verify eval**

Run:
```bash
nix eval .#nixosConfigurations.postgres.config.services.postgresql.package.version
```
Expected: `"18.4"`

- [ ] **Step 4: Commit**

```bash
git add modules/machines/postgres.nix
git commit -m "feat(postgres): enable PostgreSQL 18 with memory tuning"
```

---

### Task 3: Metabase database & user via clan.core.postgresql

**Files:**
- Modify: `modules/machines/postgres.nix` — same module

**Interfaces:**
- Produces: `clan.core.postgresql` with one user and one database
- Note: The clan-core postgresql module adds `services.postgresql.enable = true` so we can remove the direct enable from Task 2, or keep it for clarity (the lib.mkIf will merge)

- [ ] **Step 1: Add clan.core.postgresql configuration**

After the postgresql settings block, add:

```nix
clan.core.postgresql = {
  enable = true;
  users.metabase = { };
  databases.metabase = { };
};
```

Note: `clan.core.postgresql.enable = true` also sets `services.postgresql.enable = true`, so the `services.postgresql.enable` from Task 2 is redundant but harmless. Leave both for clarity.

- [ ] **Step 2: Format**

```bash
nix fmt modules/machines/postgres.nix
```

- [ ] **Step 3: Verify eval**

Run:
```bash
nix eval .#nixosConfigurations.postgres.config.clan.core.postgresql.users.metabase.name
```
Expected: `"metabase"`

- [ ] **Step 4: Commit**

```bash
git add modules/machines/postgres.nix
git commit -m "feat(postgres): create metabase user and database via clan.core.postgresql"
```

---

### Task 4: Shared clan vars generator for DB password

**Files:**
- Modify: `modules/machines/postgres.nix` — same module

**Interfaces:**
- Produces: `clan.core.vars.generators.metabase-db` with share=true, output file `password`
- Consumed by: Task 5 (oneshot service on postgres), Task 8 (metabase LoadCredential)

- [ ] **Step 1: Add the clan vars generator**

After `clan.core.postgresql`, add:

```nix
clan.core.vars.generators.metabase-db = {
  share = true;
  files.password = {
    secret = true;
    deploy = true;
    restartUnits = [ "metabase-db-password.service" ];
  };
  runtimeInputs = [ pkgs.openssl ];
  script = ''
    openssl rand -hex 32 > $out/password
  '';
};
```

- [ ] **Step 2: Format**

```bash
nix fmt modules/machines/postgres.nix
```

- [ ] **Step 3: Verify eval**

Run:
```bash
nix eval .#nixosConfigurations.postgres.config.clan.core.vars.generators.metabase-db.share
```
Expected: `true`

- [ ] **Step 4: Commit**

```bash
git add modules/machines/postgres.nix
git commit -m "feat(postgres): shared clan vars generator for metabase DB password"
```

---

### Task 5: Oneshot password-setter service

**Files:**
- Modify: `modules/machines/postgres.nix` — same module

**Interfaces:**
- Produces: systemd oneshot `metabase-db-password.service` that runs `ALTER USER metabase WITH PASSWORD` using the secret from the generator
- Consumes: `clan.core.vars.generators.metabase-db.files.password.path` (Task 4)

- [ ] **Step 1: Add the oneshot systemd service**

After the generator, add:

```nix
systemd.services.metabase-db-password = {
  description = "Set metabase database user password";
  wantedBy = [ "multi-user.target" ];
  requires = [ "postgresql.service" ];
  after = [ "postgresql.service" ];
  path = [ config.services.postgresql.package ];
  serviceConfig = {
    Type = "oneshot";
    RemainAfterExit = true;
    User = "postgres";
    Group = "postgres";
  };
  script = ''
    export PGPASSFILE=/run/postgresql/.pgpass
    psql -c "ALTER USER metabase WITH PASSWORD '$(cat ${config.clan.core.vars.generators.metabase-db.files.password.path})'"
  '';
};
```

- [ ] **Step 2: Format**

```bash
nix fmt modules/machines/postgres.nix
```

- [ ] **Step 3: Verify eval**

Run:
```bash
nix eval .#nixosConfigurations.postgres.config.systemd.services.metabase-db-password.wantedBy
```
Expected: `[ "multi-user.target" ]`

- [ ] **Step 4: Commit**

```bash
git add modules/machines/postgres.nix
git commit -m "feat(postgres): oneshot service to set metabase DB password at runtime"
```

---

### Task 6: pg_hba hostssl + PostgreSQL SSL with ACME certs + ACME client

**Files:**
- Modify: `modules/machines/postgres.nix` — both the top-level (ACME instance) and the nixos module (pg_hba, SSL, cert splitting)

**Interfaces:**
- Consumes: ACME certs pulled by the clan ACME client module (land at `/var/lib/ssl-cert/full.pem`)
- Produces: pg_hba with hostssl + host rules; PostgreSQL SSL pointing to combined PEM; ACME inventory instance

- [ ] **Step 1: Add ACME client instance at top level**

At the top level of `modules/machines/postgres.nix` (same level as `flake.deploy.nodes.postgres`), add:

```nix
clan.inventory.instances.acme-postgres = {
  roles.client.machines.postgres = {
    settings = {
      domain = "postgres.home.binarin.info";
      reloadServices = [ "postgresql.service" ];
    };
  };
};
```

- [ ] **Step 2: Add pg_hba rules in the nixos module**

In the `postgres-configuration`, add:

```nix
services.postgresql.authentication = ''
  hostssl metabase metabase 192.168.2.36/32 md5
  host    metabase metabase 100.64.0.0/10  md5
'';
```

- [ ] **Step 3: Add PostgreSQL SSL config**

After the pg_hba rules, add to the existing `services.postgresql` block:

```nix
services.postgresql = {
  ssl = "on";
  sslCertFile = "/var/lib/ssl-cert/full.pem";
  sslKeyFile = "/var/lib/ssl-cert/full.pem";
};
```

PostgreSQL reads the first certificate and first private key from the respective files.
Since both point to the same combined PEM, it extracts each section automatically —
no splitting needed.

Note: Merge these new settings into the existing `services.postgresql` block from Task 2
rather than creating a separate block.

- [ ] **Step 4: Format**

```bash
nix fmt modules/machines/postgres.nix
```

- [ ] **Step 5: Verify eval**

Run:
```bash
nix eval .#nixosConfigurations.postgres.config.services.postgresql.ssl
```
Expected: `"on"`

- [ ] **Step 6: Commit**

```bash
git add modules/machines/postgres.nix
git commit -m "feat(postgres): hostssl pg_hba, SSL with ACME certs, ACME client"
```

---

### Task 7: Metabase CT resources

**Files:**
- Modify: `modules/machines/metabase.nix` — inside `flake.nixosModules.metabase-configuration`

**Interfaces:**
- Produces: `proxmoxLXC` block with cores, memory, `/nix` mount

- [ ] **Step 1: Add proxmoxLXC configuration block**

In `flake.nixosModules.metabase-configuration`, after the `imports` list, add:

```nix
proxmoxLXC = {
  cores = 4;
  memory = 4096;
  mounts = [
    {
      mountPoint = "/nix";
      size = "32G";
      backup = false;
    }
  ];
};
```

- [ ] **Step 2: Format**

```bash
nix fmt modules/machines/metabase.nix
```

- [ ] **Step 3: Verify eval**

Run:
```bash
nix eval .#nixosConfigurations.metabase.config.proxmoxLXC.cores
```
Expected: `4`

- [ ] **Step 4: Commit**

```bash
git add modules/machines/metabase.nix
git commit -m "feat(metabase): configure CT resources — 4 cores, 4GB RAM, /nix mount"
```

---

### Task 8: Metabase service with LoadCredential + ImportCredential

**Files:**
- Modify: `modules/machines/metabase.nix` — inside `flake.nixosModules.metabase-configuration`

**Interfaces:**
- Consumes: `clan.core.vars.generators.metabase-db.files.password.path` (shared var from Task 4)
- Produces: Metabase service configured to use PostgreSQL over Tailscale

- [ ] **Step 1: Enable metabase with DB connection and credential injection**

After `proxmoxLXC`, add a `config` block:

```nix
services.metabase = {
  enable = true;
  listen.port = 3000;
};

systemd.services.metabase = {
  environment = {
    MB_DB_TYPE = "postgres";
    MB_DB_HOST = "postgres.lynx-lizard.ts.net";
    MB_DB_PORT = "5432";
    MB_DB_DBNAME = "metabase";
    MB_DB_USER = "metabase";
  };
  serviceConfig.LoadCredential = [
    "MB_DB_PASS:${config.clan.core.vars.generators.metabase-db.files.password.path}"
  ];
  serviceConfig.ImportCredential = [ "MB_DB_PASS" ];
};
```

- [ ] **Step 2: Format**

```bash
nix fmt modules/machines/metabase.nix
```

- [ ] **Step 3: Verify eval**

Run:
```bash
nix eval .#nixosConfigurations.metabase.config.services.metabase.listen.port
```
Expected: `3000`

```bash
nix eval .#nixosConfigurations.metabase.config.systemd.services.metabase.serviceConfig.LoadCredential
```
Expected output should contain the string `MB_DB_PASS:`

- [ ] **Step 4: Commit**

```bash
git add modules/machines/metabase.nix
git commit -m "feat(metabase): enable metabase service with PostgreSQL via LoadCredential"
```

---

### Task 9: nginx + Tailscale Serve for metabase web access

**Files:**
- Modify: `modules/machines/metabase.nix` — same module

**Interfaces:**
- Consumes: Metabase listening on localhost:3000 (Task 8)
- Produces: nginx with SSL + Tailscale Serve configuration

- [ ] **Step 1: Add Tailscale and nginx configuration**

After the metabase service config, add:

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

services.nginx = {
  enable = true;
  virtualHosts."metabase.home.binarin.info" = {
    forceSSL = true;
    enableACME = false;  # cert pulled by clan ACME client, not nginx native ACME
    serverAliases = [ "metabase.clan.binarin.info" ];
    locations."/" = {
      proxyPass = "http://localhost:3000";
      recommendedProxySettings = true;
    };
  };
};
```

- [ ] **Step 2: Format**

```bash
nix fmt modules/machines/metabase.nix
```

- [ ] **Step 3: Verify eval**

Run:
```bash
nix eval .#nixosConfigurations.metabase.config.services.tailscale.serve.services.metabase.target
```
Expected: `"localhost:3000"`

```bash
nix eval .#nixosConfigurations.metabase.config.services.nginx.virtualHosts."metabase.home.binarin.info".forceSSL
```
Expected: `true`

- [ ] **Step 4: Commit**

```bash
git add modules/machines/metabase.nix
git commit -m "feat(metabase): nginx reverse proxy + Tailscale Serve for web access"
```

---

### Task 10: Metabase ACME client instance + nginx SSL cert path

**Files:**
- Modify: `modules/machines/metabase.nix` — top level (ACME instance), nixos module (ssl cert paths for nginx)

**Interfaces:**
- Consumes: clan ACME client module (pulls certs to `/var/lib/ssl-cert/full.pem`)
- Produces: ACME inventory instance, nginx sslCertificate paths

- [ ] **Step 1: Add ACME client instance at top level**

At the top level of `modules/machines/metabase.nix`, add:

```nix
clan.inventory.instances.acme-metabase = {
  roles.client.machines.metabase = {
    settings = {
      domain = "metabase.home.binarin.info";
      extraDomainNames = [ "metabase.clan.binarin.info" ];
      reloadServices = [ "nginx.service" ];
    };
  };
};
```

- [ ] **Step 2: Add SSL cert paths to the nginx vhost**

In the nginx vhost block (from Task 9), add `sslCertificate` and `sslCertificateKey`:

```nix
services.nginx.virtualHosts."metabase.home.binarin.info" = {
  # ... keep existing forceSSL, enableACME, serverAliases, locations ...
  sslCertificate = "/var/lib/ssl-cert/full.pem";
  sslCertificateKey = "/var/lib/ssl-cert/full.pem";
};
```

Since the ACME client module combines cert + key in one file, point both paths to the same file. nginx handles this correctly.

- [ ] **Step 3: Format**

```bash
nix fmt modules/machines/metabase.nix
```

- [ ] **Step 4: Verify eval**

Run:
```bash
nix eval .#nixosConfigurations.metabase.config.services.nginx.virtualHosts."metabase.home.binarin.info".sslCertificate
```
Expected: `"/var/lib/ssl-cert/full.pem"`

- [ ] **Step 5: Commit**

```bash
git add modules/machines/metabase.nix
git commit -m "feat(metabase): ACME client for nginx certs"
```

---

### Task 11: End-to-end verification

- [ ] **Step 1: Format both files**

```bash
nix fmt modules/machines/postgres.nix modules/machines/metabase.nix
```

- [ ] **Step 2: Full eval of both configurations**

```bash
nix eval .#nixosConfigurations.postgres.config.system.build.toplevel.drvPath
nix eval .#nixosConfigurations.metabase.config.system.build.toplevel.drvPath
```
Expected: both return derivation paths (no errors)

- [ ] **Step 3: Verify generator on postgres machine**

```bash
clan vars generate postgres --generator metabase-db --no-regenerate
```
Expected: generates the password, outputs success message

- [ ] **Step 4: Verify no secrets in nix store**

```bash
nix build .#nixosConfigurations.postgres.config.system.build.toplevel --no-link 2>&1
```

Inspect that no path contains the generated password. The password should only exist in the clan vars store (`vars/`) and be deployed at runtime.

- [ ] **Step 5: Commit final verification adjustments**

If any issues found during verification, fix and commit. Otherwise:

```bash
git add -A
git commit -m "verify: postgres + metabase configurations eval successfully"
```
