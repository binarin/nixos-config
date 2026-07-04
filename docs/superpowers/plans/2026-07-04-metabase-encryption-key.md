# Metabase `MB_ENCRYPTION_SECRET_KEY` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Provision a permanent `MB_ENCRYPTION_SECRET_KEY` for Metabase as a machine-local clan var, injected via the (renamed) sops env template.

**Architecture:** A new non-shared `clan.core.vars.generators.metabase-encryption` generator produces a base64 secret. The existing sops env-file template (renamed `metabase-db-uri.env` → `metabase-secrets.env`) gains a second line that substitutes the key from the clan-vars sops placeholder. The template is already wired as the metabase service `EnvironmentFile`, so the secret stays in tmpfs and never enters `/nix/store`.

**Tech Stack:** Nix, clan (`clan.core.vars`), sops-nix, systemd, Metabase.

## Global Constraints

- Only `modules/machines/metabase.nix` is edited.
- Secret must never enter `/nix/store` — inject via sops placeholder into the tmpfs `EnvironmentFile` only.
- Generator is machine-local: `share` stays at its default (`false`).
- Strip openssl's trailing newline with `tr -d '\n'` (repo convention, commits `a5f3d8b9`/`dfcf408a`).
- Key is write-once/permanent — never plan a rotation step.
- Verification is `nix eval` (this repo has no unit-test framework for machine configs).

---

### Task 1: Add generator, rename template, inject the key

**Files:**
- Modify: `modules/machines/metabase.nix` (the `metabase-configuration` nixos module, `config = { ... }` block around lines 88–107)

**Interfaces:**
- Consumes: existing clan-vars sops placeholder `vars/postgresql-postgres-metabase-metabase/password` (unchanged); `pkgs.openssl`.
- Produces:
  - `config.clan.core.vars.generators.metabase-encryption.files.secret-key` — the new secret file.
  - sops placeholder `vars/metabase-encryption/secret-key` — used in the env template.
  - `config.sops.templates."metabase-secrets.env"` — replaces the old `"metabase-db-uri.env"` template (same path, renamed).

- [ ] **Step 1: Add the `metabase-encryption` generator**

Insert this block inside the `config = { ... }` of the `metabase-configuration` module (e.g. immediately before the `sops.templates` block at current line 95). Note `pkgs` is already the module's arg (`{ config, lib, pkgs, ... }:`).

```nix
        clan.core.vars.generators.metabase-encryption = {
          files.secret-key = {
            secret = true;
            deploy = true;
            restartUnits = [ "metabase.service" ];
          };
          runtimeInputs = [ pkgs.openssl ];
          # Metabase's docs suggest `openssl rand -base64 32`; tr -d strips the
          # trailing newline so the env value is clean.
          script = ''
            openssl rand -base64 32 | tr -d '\n' > $out/secret-key
          '';
        };
```

- [ ] **Step 2: Rename the sops template and add the key line**

Replace the existing template block (current lines 95–102):

```nix
        sops.templates."metabase-db-uri.env" = {
          restartUnits = [ "metabase.service" ];
          content = ''
            MB_DB_CONNECTION_URI=postgres://postgres.lynx-lizard.ts.net:5432/metabase?user=metabase&password=${
              config.sops.placeholder."vars/postgresql-postgres-metabase-metabase/password"
            }&sslmode=require
          '';
        };
```

with:

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
```

- [ ] **Step 3: Update the `EnvironmentFile` reference**

Change the reference (current line 105) from the old template name to the new one:

```nix
        systemd.services.metabase.serviceConfig.EnvironmentFile = [
          config.sops.templates."metabase-secrets.env".path
        ];
```

Confirm no other occurrence of `metabase-db-uri.env` remains:

Run: `grep -n "metabase-db-uri.env" modules/machines/metabase.nix`
Expected: no output (empty).

- [ ] **Step 4: Evaluate the machine config**

Run: `nix eval .#nixosConfigurations.metabase.config.system.build.toplevel`
Expected: PASS — resolves to a `/nix/store/...-nixos-system-...` path with no eval error. (A missing-secret warning about ungenerated vars is acceptable at this stage; a `.#` eval *error* is not.)

- [ ] **Step 5: Generate the encryption key var**

Run: `clan vars generate metabase --generator metabase-encryption --no-regenerate`
Expected: creates the encrypted var; a subsequent `clan vars list metabase` shows `metabase-encryption/secret-key`.

- [ ] **Step 6: Confirm the secret stays out of the store**

Run: `nix eval --raw .#nixosConfigurations.metabase.config.sops.templates."metabase-secrets.env".path`
Expected: a path under `/run/` (tmpfs), not `/nix/store/`.

- [ ] **Step 7: Commit**

```bash
git add modules/machines/metabase.nix
git commit -m "feat(metabase): add MB_ENCRYPTION_SECRET_KEY clan var

Rename metabase-db-uri.env sops template to metabase-secrets.env and
inject a permanent base64 encryption key from a new machine-local
clan vars generator (metabase-encryption).

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Self-Review

**Spec coverage:**
- §2 generator → Task 1 Step 1. ✔ (non-shared, base64, `tr -d '\n'`, restartUnits)
- §3 rename + inject via placeholder → Task 1 Steps 2–3. ✔
- §4 files touched (metabase.nix only) → Global Constraints + Task 1 Files. ✔
- §5 verification (eval, generate, tmpfs check) → Task 1 Steps 4–6. ✔

**Placeholder scan:** No TBD/TODO; all code shown verbatim. ✔

**Type consistency:** Template name `metabase-secrets.env` used identically in Steps 2, 3, 6; placeholder `vars/metabase-encryption/secret-key` matches generator name `metabase-encryption` + file `secret-key`. ✔
