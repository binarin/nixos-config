# Metabase `MB_ENCRYPTION_SECRET_KEY` Design

**Date:** 2026-07-04
**Status:** Design approved

## 1. Overview

Add `MB_ENCRYPTION_SECRET_KEY` to the `metabase` machine. Metabase uses this key to
encrypt sensitive values (database connection details, integration credentials, etc.)
stored in its application database. The key is a **write-once, permanent** secret:
generated once via a clan var, then never rotated — rotating it orphans every
previously-encrypted value in the app DB.

The key is provisioned as a machine-local clan var and injected into the metabase
service via the existing sops env-file template, so it never enters `/nix/store`.

## 2. Clan Var Generator

A new machine-local generator in the `metabase-configuration` module:

```nix
clan.core.vars.generators.metabase-encryption = {
  files.secret-key = {
    secret = true;
    deploy = true;
    restartUnits = [ "metabase.service" ];
  };
  runtimeInputs = [ pkgs.openssl ];
  script = ''
    openssl rand -base64 32 | tr -d '\n' > $out/secret-key
  '';
};
```

- **Not shared** (`share` defaults to `false`) — only `metabase` consumes this key,
  unlike the DB password (which both `postgres` and `metabase` decrypt).
- **`openssl rand -base64 32`** — the exact form Metabase's own documentation suggests.
- **`tr -d '\n'`** strips openssl's trailing newline, consistent with the repo's recent
  fixes (`a5f3d8b9`, `dfcf408a`); otherwise the env value would carry a stray newline.
- Write-once: generated on the first `clan vars generate`, then stable forever.

## 3. Injection (existing sops template, renamed)

The current template `sops.templates."metabase-db-uri.env"` is **renamed** to
`metabase-secrets.env` (it now carries more than the DB URI) and extended with a second
line. The encryption key is substituted from the clan-vars sops placeholder — the same
mechanism already used for the DB password:

```nix
sops.templates."metabase-secrets.env" = {
  restartUnits = [ "metabase.service" ];
  content = ''
    MB_DB_CONNECTION_URI=postgres://postgres.lynx-lizard.ts.net:5432/metabase?user=metabase&password=${
      config.sops.placeholder."vars/postgresql-postgres-metabase-metabase/password"
    }&sslmode=require
    MB_ENCRYPTION_SECRET_KEY=${config.sops.placeholder."vars/metabase-encryption/secret-key"}
  '';
};

systemd.services.metabase.serviceConfig.EnvironmentFile = [
  config.sops.templates."metabase-secrets.env".path
];
```

- The template already lists `restartUnits = [ "metabase.service" ]` and is already
  wired as the service `EnvironmentFile` — no additional service wiring is needed.
- Base64's `+`, `/`, `=` characters are safe as an unquoted `EnvironmentFile` value
  (systemd reads the value literally to end-of-line).
- The rendered file lives under `/run` (tmpfs) — the key never touches `/nix/store`.

## 4. Files Touched

- `modules/machines/metabase.nix` only:
  - add the `metabase-encryption` generator,
  - rename the sops template and add the `MB_ENCRYPTION_SECRET_KEY` line,
  - update the `EnvironmentFile` reference to the renamed template.

## 5. Verification

- `nix eval .#nixosConfigurations.metabase.config.system.build.toplevel` — machine evals.
- `clan vars generate metabase --generator metabase-encryption --no-regenerate` —
  produces the key in the encrypted clan-vars store.
- Confirm the rendered `EnvironmentFile` path resolves under `/run` (tmpfs) and that
  the key does not appear in any `/nix/store` path.
