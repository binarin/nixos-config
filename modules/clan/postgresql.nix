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
                role
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
      manifest.readme = ''
        A PostgreSQL server (`server` role) provisions a role + database + password
        for each consumer (`client` role). Passwords are `share = true` clan vars
        generators (one per `(database, user)` pair) declared in `perMachine`, so the
        server and the owning client decrypt the same secret. Secrets are injected via
        sops templates (rendered to tmpfs) — never into `/nix/store` or `psql` argv.

        The server sets each password at runtime (`ALTER USER`), owns SSL/listen
        config, and emits `hostssl … scram-sha-256` pg_hba lines per client
        `sourceCIDRs`. A client declares its `(database, user)` pairs via `access.<label>`
        and references the shared password path (or the sops placeholder) itself.
      '';

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
                        ${lib.optionalString (
                          e.role == "owner"
                        ) ''ALTER DATABASE "${e.database}" OWNER TO "${e.user}";''}
                        ${grantSql e (ownerByDb.${e.database} or null)}
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

                assertions = map (db: {
                  assertion = lib.length (ownersByDb.${db} or [ ]) == 1;
                  message =
                    let
                      owners = ownersByDb.${db} or [ ];
                    in
                    "postgresql clan module: database '${db}' must have exactly one access entry with role = \"owner\" (found ${toString (lib.length owners)}${lib.optionalString (owners != [ ]) ": ${lib.concatStringsSep ", " owners}"}).";
                }) databases;
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
                      # tr -d strips openssl's trailing newline; otherwise the password
                      # value (and every ALTER USER '…' SQL literal it's substituted into)
                      # would carry a stray newline.
                      script = "openssl rand -hex 32 | tr -d '\\n' > $out/password";
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
