# PostgreSQL user/password clan module — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the bespoke metabase DB password wiring with a reusable `postgresql` clan service module (server/client roles + `perMachine` shared generators) and migrate postgres + metabase onto it.

**Architecture:** A `clan.service` module `flake.clan.modules.postgresql` with a `server` role (builds on `clan.core.postgresql`; adds runtime password setting via a sops-rendered SQL file, `hostssl` pg_hba, SSL/listen config), a thin `client` role (declares the access pairs), and a `perMachine` block declaring the `share = true` password generators — one per `(database, user)` — following the clan-core `certificates`/`zerotier` pattern. Secrets are injected into rendered files with **sops templates** (clan vars sit on sops-nix), so the plaintext never touches `/nix/store`. A single instance named `postgres` binds the postgres host as `server` and each consumer as `client`.

**Tech Stack:** Nix, flake-parts (dendritic — `import-tree ./modules`), clan / clan.core vars, sops-nix (templates + placeholder), NixOS systemd, PostgreSQL 18, Metabase.

## Global Constraints

- **Secrets never reach `/nix/store`.** Passwords flow only through clan vars secret files and sops-rendered templates (tmpfs `/run`). Never interpolate a password into a Nix string, `initialScript`, `psql` argv, or a store path.
- **clan vars → sops:** every deployed secret file becomes `sops.secrets."vars/<generator>/<file>"` (because `neededFor` defaults to `"services"`). Its value is available in templates as `config.sops.placeholder."vars/<generator>/<file>"`. `sops.placeholder` is only populated once at least one `sops.templates.*` is defined **and** the secret file exists at eval time — hence `clan vars generate` must run before evaluating any config that reads the placeholder.
- **Auth is `scram-sha-256` only.** No `md5`. PG18 already defaults `password_encryption = scram-sha-256`.
- **SSL is mandatory.** The server emits `hostssl` (never `host`) pg_hba lines. Consumers connect with SSL.
- **Each `(database, user)` pair gets its own generator** named `postgresql-${instanceName}-${database}-${user}` with `share = true`.
- **After editing any `.nix` file, run `nix fmt <file>`** before committing (repo convention, commit `f44298eb`).
- **Verification is by evaluation:** `nix eval .#nixosConfigurations.<machine>.config.<attr>` for pinpoint checks; `ncf eval nixos <machine>` for a full-config eval (same `nix eval` underneath). An evaluated config value IS the test.
- **Instance name is `postgres`; the metabase generator is `postgresql-postgres-metabase-metabase`.**

---

## File Structure

- **Create** `modules/clan/postgresql.nix` — the clan service module (`flake.clan.modules.postgresql`).
- **Modify** `modules/machines/postgres.nix` — drop the hand-written `clan.core.postgresql` block, `metabase-db-password` service, `authentication` lines, SSL/listen settings, and the `metabase-db-generator` import; add `postgres` instance `server` membership. Keep LXC, package, memory tuning.
- **Modify** `modules/machines/metabase.nix` — join the `postgres` instance as `client` with `access.metabase`; connect via a sops-templated `MB_DB_CONNECTION_URI` `EnvironmentFile` (`sslmode=require`); drop the `metabase-db-generator` import and the old `MB_DB_*`/`LoadCredential` wiring.
- **Delete** `modules/machines/metabase-db.nix` — the old `metabase-db-generator` module.

---

## Task 1: Create the `postgresql` clan service module

**Files:**
- Create: `modules/clan/postgresql.nix`

**Interfaces:**
- Produces: `flake.clan.modules.postgresql` — a `clan.service` module with:
  - `roles.server.interface`: option `certPath` (str, default `/var/lib/ssl-cert/full.pem`).
  - `roles.client.interface`: option `access` = attrsOf submodule `{ database, user, owner, sourceCIDRs, restartUnits, initSql, secret = { owner, group, mode } }`.
  - `perMachine` declaring generators `postgresql-<instance>-<database>-<user>` (`share = true`, file `password`).
  - server `perInstance` setting `clan.core.postgresql.{enable,users,databases}`, `services.postgresql.settings` (ssl/listen), `services.postgresql.authentication` (hostssl lines), a `sops.templates."postgresql-provision-<db>-<user>.sql"` per pair, a `postgresql-provision-<db>-<user>` oneshot per pair (`psql -f` the rendered SQL), and an `assertions` entry (≤1 owner per database).
- Consumed by: Tasks 2 and 3 (via the `postgres` inventory instance).

- [ ] **Step 1: Write the module file**

Create `modules/clan/postgresql.nix` with exactly this content:

```nix
{ ... }:
{
  flake.clan.modules.postgresql =
    { lib, ... }:
    let
      genName = instanceName: database: user: "postgresql-${instanceName}-${database}-${user}";
      secretName = instanceName: database: user: "vars/${genName instanceName database user}/password";

      # Flatten a client-machines attrset (machineName -> { settings.access = { <label> -> entry }; })
      # into a flat list of entries carrying their machine name.
      flattenClients =
        clientMachines:
        lib.concatLists (
          lib.mapAttrsToList (
            machineName: m:
            lib.mapAttrsToList (label: entry: {
              machine = machineName;
              inherit label;
              inherit (entry)
                database
                user
                owner
                sourceCIDRs
                restartUnits
                initSql
                secret
                ;
            }) (m.settings.access or { })
          ) clientMachines
        );
    in
    {
      _class = "clan.service";
      manifest.name = "nixos-config-postgresql";
      manifest.description = "Provision PostgreSQL roles/databases with shared passwords distributed to consumer machines";

      roles.client = {
        description = "A machine that consumes one or more (database, user) pairs, receiving each shared password.";
        interface =
          { lib, ... }:
          {
            options.access = lib.mkOption {
              default = { };
              description = "PostgreSQL (database, user) pairs this machine connects as, keyed by an arbitrary label.";
              type = lib.types.attrsOf (
                lib.types.submodule (
                  { name, ... }:
                  {
                    options = {
                      database = lib.mkOption {
                        type = lib.types.str;
                        default = name;
                        description = "Database name.";
                      };
                      user = lib.mkOption {
                        type = lib.types.str;
                        default = name;
                        description = "Role name that connects to the database.";
                      };
                      owner = lib.mkOption {
                        type = lib.types.bool;
                        default = false;
                        description = "Whether this role owns the database (gets ALTER DATABASE ... OWNER TO).";
                      };
                      sourceCIDRs = lib.mkOption {
                        type = lib.types.listOf lib.types.str;
                        default = [ ];
                        description = "Source networks this machine connects from; each becomes a hostssl pg_hba line.";
                      };
                      restartUnits = lib.mkOption {
                        type = lib.types.listOf lib.types.str;
                        default = [ ];
                        description = "Consumer systemd units to restart when the password rotates.";
                      };
                      initSql = lib.mkOption {
                        type = lib.types.lines;
                        default = "";
                        description = "Raw SQL run (as postgres) after the password is set — GRANTs etc.";
                      };
                      secret = {
                        owner = lib.mkOption {
                          type = lib.types.str;
                          default = "root";
                          description = "Owner of the deployed password file on this machine (for consumers that read it directly).";
                        };
                        group = lib.mkOption {
                          type = lib.types.str;
                          default = "root";
                          description = "Group of the deployed password file on this machine.";
                        };
                        mode = lib.mkOption {
                          type = lib.types.str;
                          default = "0400";
                          description = "Mode of the deployed password file on this machine.";
                        };
                      };
                    };
                  }
                )
              );
            };
          };

        # Thin: the shared generators live in perMachine; a consumer references either
        # the deployed password path or a sops template placeholder itself.
        perInstance = { ... }: { nixosModule = { }; };
      };

      roles.server = {
        description = "The PostgreSQL host: provisions every client's role, database, password, pg_hba and SSL.";
        interface =
          { lib, ... }:
          {
            options.certPath = lib.mkOption {
              type = lib.types.str;
              default = "/var/lib/ssl-cert/full.pem";
              description = "Combined PEM (cert+key) used for ssl_cert_file/ssl_key_file.";
            };
          };

        perInstance =
          {
            instanceName,
            settings,
            roles,
            ...
          }:
          {
            nixosModule =
              {
                config,
                lib,
                pkgs,
                ...
              }:
              let
                entries = flattenClients (roles.client.machines or { });
                databases = lib.unique (map (e: e.database) entries);
                users = lib.unique (map (e: e.user) entries);
                ownersByDb = lib.mapAttrs (_db: es: map (e: e.user) (lib.filter (e: e.owner) es)) (
                  lib.groupBy (e: e.database) entries
                );
                unitName = e: "postgresql-provision-${e.database}-${e.user}";
                tmplName = e: "postgresql-provision-${e.database}-${e.user}.sql";
              in
              {
                # Build on clan-core: idempotent CREATE USER / CREATE DATABASE + backup hooks.
                clan.core.postgresql = {
                  enable = true;
                  users = lib.genAttrs users (_u: { });
                  databases = lib.genAttrs databases (_d: { });
                };

                # SSL + listen; certs come from the acme client role already on this machine.
                services.postgresql.settings = {
                  ssl = "on";
                  ssl_cert_file = settings.certPath;
                  ssl_key_file = settings.certPath;
                  listen_addresses = lib.mkForce "*";
                };

                # pg_hba: one hostssl line per (db, user, cidr). scram-sha-256 only.
                services.postgresql.authentication = lib.mkAfter (
                  lib.concatMapStringsSep "\n" (
                    e:
                    lib.concatMapStringsSep "\n" (
                      cidr: "hostssl ${e.database} ${e.user} ${cidr} scram-sha-256"
                    ) e.sourceCIDRs
                  ) entries
                );

                # Render each pair's provisioning SQL via a sops template: the password is
                # substituted from the placeholder at activation into a tmpfs file owned by
                # postgres — never into /nix/store and never into psql argv.
                sops.templates = lib.listToAttrs (
                  map (
                    e:
                    lib.nameValuePair (tmplName e) {
                      owner = "postgres";
                      restartUnits = [ "${unitName e}.service" ];
                      content = ''
                        ALTER USER "${e.user}" WITH PASSWORD '${
                          config.sops.placeholder."${secretName instanceName e.database e.user}"
                        }';
                        ${lib.optionalString e.owner ''ALTER DATABASE "${e.database}" OWNER TO "${e.user}";''}
                        ${e.initSql}
                      '';
                    }
                  ) entries
                );

                # One provisioning oneshot per (db, user): apply the rendered SQL as postgres.
                systemd.services = lib.listToAttrs (
                  map (
                    e:
                    lib.nameValuePair (unitName e) {
                      description = "Provision PostgreSQL role ${e.user} / database ${e.database}";
                      wantedBy = [ "multi-user.target" ];
                      requires = [ "postgresql.service" ];
                      after = [ "postgresql.service" ];
                      path = [
                        config.services.postgresql.package
                        pkgs.util-linux
                      ];
                      serviceConfig = {
                        Type = "oneshot";
                        RemainAfterExit = true;
                      };
                      script = ''
                        set -euo pipefail
                        runuser -u postgres -- psql -v ON_ERROR_STOP=1 --no-psqlrc -f ${
                          config.sops.templates."${tmplName e}".path
                        }
                      '';
                    }
                  ) entries
                );

                assertions = lib.mapAttrsToList (db: owners: {
                  assertion = lib.length owners <= 1;
                  message = "postgresql clan module: database '${db}' has multiple owners (${lib.concatStringsSep ", " owners}); at most one access entry per database may set owner = true.";
                }) ownersByDb;
              };
          };
      };

      # Shared password generators live here — one definition, evaluated per machine.
      perMachine =
        { instances, machine, ... }:
        {
          nixosModule =
            { lib, pkgs, ... }:
            {
              clan.core.vars.generators = lib.mkMerge (
                lib.mapAttrsToList (
                  instanceName: inst:
                  let
                    clientMachines = inst.roles.client.machines or { };
                    isServer = builtins.elem "server" machine.roles;
                    allEntries = flattenClients clientMachines;
                    myEntries = flattenClients (lib.filterAttrs (n: _: n == machine.name) clientMachines);

                    baseGen = {
                      share = true;
                      runtimeInputs = [ pkgs.openssl ];
                      script = "openssl rand -hex 32 > $out/password";
                    };

                    serverGen =
                      e:
                      lib.nameValuePair (genName instanceName e.database e.user) (
                        baseGen
                        // {
                          files.password = {
                            secret = true;
                            deploy = true;
                            restartUnits = [ "postgresql-provision-${e.database}-${e.user}.service" ];
                          };
                        }
                      );

                    clientGen =
                      e:
                      lib.nameValuePair (genName instanceName e.database e.user) (
                        baseGen
                        // {
                          files.password = {
                            secret = true;
                            deploy = true;
                            owner = e.secret.owner;
                            group = e.secret.group;
                            mode = e.secret.mode;
                            restartUnits = e.restartUnits;
                          };
                        }
                      );
                  in
                  lib.listToAttrs (
                    (lib.optionals isServer (map serverGen allEntries)) ++ (map clientGen myEntries)
                  )
                ) instances
              );
            };
        };
    };
}
```

- [ ] **Step 2: Format the file**

Run: `nix fmt modules/clan/postgresql.nix`
Expected: exits 0, no diff complaints.

- [ ] **Step 3: Verify the module parses and is registered**

Run: `nix eval .#clan.modules --apply 'ms: builtins.hasAttr "postgresql" ms'`
Expected: `true`

(If `.#clan.modules` is not evaluable in this flake, fall back to `nix-instantiate --parse modules/clan/postgresql.nix >/dev/null && echo OK` — Expected: `OK`. The real functional check happens in Task 2.)

- [ ] **Step 4: Commit**

```bash
git add modules/clan/postgresql.nix
git commit -m "feat(postgresql): add clan service module for DB user/password sharing

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: Wire the `postgres` instance, migrate `postgres.nix`, generate the shared secret

Wiring + machine migration happen together (the module's `lib.mkForce listen_addresses` would otherwise clash with the machine's). The shared secret is generated at the end so the server's sops placeholder resolves and the config evaluates.

**Files:**
- Modify: `modules/machines/postgres.nix`
- Create (generated): `vars/shared/postgresql-postgres-metabase-metabase/`

**Interfaces:**
- Consumes: `flake.clan.modules.postgresql` (Task 1).
- Produces: inventory instance `postgres` with `roles.server.machines.postgres`; on the postgres config: generator + sops template + service `postgresql-provision-metabase-metabase`, `clan.core.postgresql.users.metabase`, `services.postgresql.settings.ssl = "on"`.

- [ ] **Step 1: Add the instance wiring (server role) at the top level**

In `modules/machines/postgres.nix`, immediately after the existing `clan.inventory.machines.postgres = { ... };` block, add the full instance definition — both the `server` (postgres) and the `client` (metabase) roles. The client must be declared here so the `postgresql-postgres-metabase-metabase` generator exists for `clan vars generate` and the eval checks below; `metabase.nix` (Task 3) only changes how metabase *connects*, not the instance membership.

```nix
  clan.inventory.instances.postgres = {
    module = {
      input = "self";
      name = "postgresql";
    };
    roles.server.machines.postgres = { };
    roles.client.machines.metabase.settings.access.metabase = {
      owner = true;
      sourceCIDRs = [
        "100.64.0.0/10"
        "192.168.2.36/32"
      ];
      restartUnits = [ "metabase.service" ];
    };
  };
```

- [ ] **Step 2: Remove the `metabase-db-generator` import**

In the `flake.nixosModules.postgres-configuration` `imports = [ ... ]` list, delete the line:

```nix
        self.nixosModules.metabase-db-generator
```

- [ ] **Step 3: Strip the now-module-owned PostgreSQL config**

Replace this exact span:

```nix
      services.postgresql = {
        enable = true;
        package = pkgs.postgresql_18;
        settings = {
          shared_buffers = "2GB";
          effective_cache_size = "6GB";
          listen_addresses = lib.mkForce "*";
          ssl = "on";
          ssl_cert_file = "/var/lib/ssl-cert/full.pem";
          ssl_key_file = "/var/lib/ssl-cert/full.pem";
        };
      };

      services.postgresql.authentication = ''
        hostssl metabase metabase 192.168.2.36/32 scram-sha-256
        host    metabase metabase 100.64.0.0/10  scram-sha-256
      '';

      clan.core.postgresql = {
        enable = true;
        users.metabase = { };
        databases.metabase = { };
      };

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
          psql -c "ALTER USER metabase WITH PASSWORD '$(cat ${config.clan.core.vars.generators.metabase-db.files.password.path})'"
        '';
      };
```

with just the machine-specific package + tuning (the module now owns `enable`, ssl, listen, authentication, users/databases, and password-setting):

```nix
      services.postgresql = {
        package = pkgs.postgresql_18;
        settings = {
          shared_buffers = "2GB";
          effective_cache_size = "6GB";
        };
      };
```

- [ ] **Step 4: Format**

Run: `nix fmt modules/machines/postgres.nix`
Expected: exits 0.

- [ ] **Step 5: Generate the shared password secret (so the placeholder resolves)**

```bash
clan vars generate postgres
```

Expected: creates `vars/shared/postgresql-postgres-metabase-metabase/password/` (a `secret` plus a `machines/postgres` entry), mirroring the recent `vars: update via generator ...` commits.

Run: `ls vars/shared/postgresql-postgres-metabase-metabase/password/machines`
Expected: contains `postgres`.

- [ ] **Step 6: Verify the module now drives the postgres config**

Run: `nix eval .#nixosConfigurations.postgres.config.clan.core.vars.generators.\"postgresql-postgres-metabase-metabase\".share`
Expected: `true`

Run: `nix eval .#nixosConfigurations.postgres.config.systemd.services.\"postgresql-provision-metabase-metabase\".serviceConfig.Type`
Expected: `"oneshot"`

Run: `nix eval .#nixosConfigurations.postgres.config.sops.templates.\"postgresql-provision-metabase-metabase.sql\".owner`
Expected: `"postgres"`

Run: `nix eval --raw .#nixosConfigurations.postgres.config.sops.templates.\"postgresql-provision-metabase-metabase.sql\".content`
Expected: contains `ALTER USER "metabase" WITH PASSWORD` and `ALTER DATABASE "metabase" OWNER TO "metabase";` (the latter because `owner = true`).

Run: `nix eval .#nixosConfigurations.postgres.config.services.postgresql.settings.ssl`
Expected: `"on"`

Run: `nix eval --raw .#nixosConfigurations.postgres.config.services.postgresql.authentication`
Expected: output contains `hostssl metabase metabase 100.64.0.0/10 scram-sha-256` (and the `192.168.2.36/32` line).

- [ ] **Step 7: Full-config eval smoke test**

Run: `ncf eval nixos postgres`
Expected: completes without error.

- [ ] **Step 8: Commit**

```bash
git add modules/machines/postgres.nix vars/shared/postgresql-postgres-metabase-metabase
git commit -m "refactor(postgres): adopt postgresql clan module for metabase provisioning

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: Migrate `metabase.nix` onto the client role with sops-templated SSL URI

Metabase's discrete `MB_DB_*` env vars do not support SSL — the app DB needs `MB_DB_CONNECTION_URI`. We render that URI (with the password substituted from the sops placeholder) into an `EnvironmentFile` on tmpfs and hand it to the metabase unit. `sslmode=require` keeps the existing tailnet host working without cert/hostname validation. No ExecStart override; no password in `/nix/store`.

**Files:**
- Modify: `modules/machines/metabase.nix`
- Update (generated): `vars/shared/postgresql-postgres-metabase-metabase/` (adds `machines/metabase`)

**Interfaces:**
- Consumes: generator + secret `postgresql-postgres-metabase-metabase` (Task 2 — the metabase client membership is already declared in `postgres.nix`).
- Produces: metabase service reading `MB_DB_CONNECTION_URI` from a sops-templated `EnvironmentFile`.

Note: the metabase client membership (`roles.client.machines.metabase` with `access.metabase`) was declared in `postgres.nix` in Task 2. Do NOT re-declare it here — this task only changes how metabase *connects*.

- [ ] **Step 1: Remove the `metabase-db-generator` import**

In the `flake.nixosModules.metabase-configuration` `imports = [ ... ]` list, delete the line:

```nix
        self.nixosModules.metabase-db-generator
```

- [ ] **Step 2: Switch the metabase service to a sops-templated connection URI**

Replace this exact block:

```nix
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

with:

```nix
        # SSL requires MB_DB_CONNECTION_URI; render it (with the password from the sops
        # placeholder) into a tmpfs EnvironmentFile so the secret never enters the store.
        sops.templates."metabase-db-uri.env" = {
          restartUnits = [ "metabase.service" ];
          content = ''
            MB_DB_CONNECTION_URI=postgres://postgres.lynx-lizard.ts.net:5432/metabase?user=metabase&password=${
              config.sops.placeholder."vars/postgresql-postgres-metabase-metabase/password"
            }&sslmode=require
          '';
        };

        systemd.services.metabase.serviceConfig.EnvironmentFile = [
          config.sops.templates."metabase-db-uri.env".path
        ];
```

- [ ] **Step 3: Format**

Run: `nix fmt modules/machines/metabase.nix`
Expected: exits 0.

- [ ] **Step 4: Re-key the shared secret so metabase can decrypt it**

```bash
clan vars generate metabase
```

Expected: adds `vars/shared/postgresql-postgres-metabase-metabase/password/machines/metabase` (metabase's key added to the shared secret).

Run: `ls vars/shared/postgresql-postgres-metabase-metabase/password/machines`
Expected: contains both `metabase` and `postgres`.

- [ ] **Step 5: Verify the metabase config**

Run: `nix eval --raw .#nixosConfigurations.metabase.config.sops.templates.\"metabase-db-uri.env\".content`
Expected: contains `MB_DB_CONNECTION_URI=postgres://postgres.lynx-lizard.ts.net:5432/metabase?user=metabase&password=<SOPS:` and ends with `&sslmode=require`.

Run: `nix eval --json .#nixosConfigurations.metabase.config.systemd.services.metabase.serviceConfig.EnvironmentFile`
Expected: a JSON array whose element is the rendered template path (contains `metabase-db-uri.env`).

- [ ] **Step 6: Full-config eval smoke test**

Run: `ncf eval nixos metabase`
Expected: completes without error.

- [ ] **Step 7: Commit**

```bash
git add modules/machines/metabase.nix vars/shared/postgresql-postgres-metabase-metabase
git commit -m "refactor(metabase): connect via postgresql clan module with mandatory SSL

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: Delete the old generator module and secret, final verify

**Files:**
- Delete: `modules/machines/metabase-db.nix`
- Delete (secret store): `vars/shared/metabase-db/`

- [ ] **Step 1: Confirm nothing still references the old generator, then delete it**

Run: `grep -rn "metabase-db-generator\|generators.metabase-db\|nixosModules.metabase-db" modules/`
Expected: no output.

Then:

```bash
git rm modules/machines/metabase-db.nix
git rm -r vars/shared/metabase-db
```

- [ ] **Step 2: Verify both machines still evaluate**

Run: `ncf eval nixos postgres`
Expected: completes without error.

Run: `ncf eval nixos metabase`
Expected: completes without error.

- [ ] **Step 3: Commit**

```bash
git add -A
git commit -m "chore(postgresql): drop superseded metabase-db generator and its shared secret

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 4: (Deploy — performed by the operator, outside this plan)**

Deploy postgres first (provisions role/db/password/pg_hba/SSL), then metabase:

```bash
nixos-rebuild switch --flake .#postgres --target-host root@<postgres>
nixos-rebuild switch --flake .#metabase --target-host root@<metabase>
```

Post-deploy checks (operator): on postgres, `journalctl -u postgresql-provision-metabase-metabase` shows a clean run; metabase connects (`journalctl -u metabase` shows no auth/SSL errors and the app loads).

---

## Notes / decisions

- **sops templates over runtime shell wrappers:** clan vars are sops-nix secrets, so both the server's `ALTER USER … PASSWORD` SQL and metabase's `MB_DB_CONNECTION_URI` are rendered by sops templates using `config.sops.placeholder."vars/postgresql-postgres-metabase-metabase/password"`. The store holds only the placeholder token; the plaintext lives only in tmpfs (`/run`), and never appears in `psql` argv.
- **`sslmode=require`** (not `verify-ca`/`verify-full`): encrypts without cert/hostname validation, so metabase keeps the `postgres.lynx-lizard.ts.net` tailnet host (whose name does not match the acme cert). The link is already inside the tailscale tunnel; this satisfies "SSL mandatory". Hardening to `verify-ca` is a future option.
- **Generate-before-eval ordering:** `sops.placeholder` is only populated for secrets that exist at eval time, so `clan vars generate` runs inside Tasks 2 and 3 before their evals — not deferred to the end.
- **Owner assertion** guards against two `access` entries claiming `owner = true` on one database; non-owner roles that need privileges use the per-entry `initSql` escape hatch (GRANTs).

## Self-Review

- **Spec coverage:** module shape (Task 1) ✓; server role — clan.core.postgresql build-on, ssl/listen, hostssl pg_hba, per-pair provisioning (sops-rendered SQL) oneshot, owner assertion (Task 1) ✓; client role — access submodule incl. `secret.{owner,group,mode}` (Task 1), sops-templated consumption (Task 3) ✓; perMachine shared generators with server-declares-all / client-declares-own branching (Task 1) ✓; migrations (Tasks 2–4) ✓; secrets-never-in-store / scram-sha-256 / SSL-mandatory constraints ✓.
- **Placeholder scan:** none — all code and commands concrete.
- **Type consistency:** generator name `postgresql-postgres-metabase-metabase`, secret `vars/postgresql-postgres-metabase-metabase/password`, provision unit/template `postgresql-provision-metabase-metabase[.sql]` used identically across Tasks 1–4; `access` submodule fields match between interface (Task 1), `flattenClients` (Task 1), and metabase's `access.metabase` (Task 3).
