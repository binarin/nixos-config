# `nix.accessTokens` clan-vars backend

## Problem

`nixos-config.nix.accessTokens` renders a nix `extra-access-tokens` line so that
flake-input fetches from `github.com` are authenticated (higher rate limits,
private repos). Today the option maps each site to a **sops secret name**, and
the token is stored per-machine in `secrets/<host>/secrets.yaml`.

After clanifying `nix-builder-raum` and `nix-builder-valak`, their forgejo
Actions runners fail to fetch github flake inputs. Investigation (root SSH to
both hosts):

- The rendered token file (`/run/secrets/rendered/nix-access-tokens`) exists and
  is group-readable by `nix-access-tokens` (mode `0440`). Plumbing is intact.
- The token **stored on raum/valak is stale** — `api.github.com` returns
  `401 Bad credentials` for it. The operator's real PAT is valid until
  **Feb 1 2027**. So this is **token drift**, not expiry: the PAT is duplicated
  per-machine in sops and the builders' copies diverged from the good one
  (regenerated/wiped during clanification).
- It surfaces "only in runners" because flake-input fetching runs in the **nix
  CLI as the invoking user** (the `forgejo-runner` DynamicUser), not the root
  daemon. That user reads `/etc/nix/nix.conf` → `!include <token file>`, so the
  token file must be group-readable — which is exactly why the group exists and
  why the runner sets `supplementaryGroups = [ "nix-access-tokens" ]`.

## Goal

On **clan machines**, back `nix.accessTokens` with a **shared clan-vars
generator** (single source of truth, no drift) while keeping the existing
sops backend for **non-clan machines** (furfur). Interface unchanged; callers
untouched.

## Consumers

- `furfur` — not a clan machine → keeps the sops backend.
- `nix-builder-raum`, `nix-builder-valak` — clan machines → clan-vars backend.

## Design

All changes are in `modules/nix.nix`, plus removal of the now-stale sops entries.

### Backend selection at the imports level (not `mkIf`)

The backend is chosen **outside config eval**, in the module's `imports` list,
keyed on `specialArgs ? clan-core` — exactly how `modules/baseline/default.nix`
conditionally pulls in `clan-baseline`:

```nix
# modules/baseline/default.nix
imports = [ ... ] ++ (lib.optional (specialArgs ? clan-core) self.nixosModules.clan-baseline);
```

This matters for two reasons an in-config `mkIf` cannot satisfy:

- On a non-clan machine the `clan.*` option does not exist, so even a
  condition-false `mkIf { clan.core... = ...; }` errors (the option path is still
  materialized). Selecting the module at import time means `clan.*` is never
  referenced on non-clan machines.
- Deciding list membership from `config` (e.g. `hasTokens`) is a module-fixpoint
  infinite recursion. `specialArgs` is static, so it is safe.

The `nix` module is therefore **backend-neutral** and imports exactly one
companion:

```nix
imports = [ inputs.determinate.nixosModules.default ]
  ++ [ (if specialArgs ? clan-core
        then self.nixosModules.nix-access-tokens-clan
        else self.nixosModules.nix-access-tokens-sops) ];
```

### Interface — unchanged

```nix
nixos-config.nix.accessTokens = { "<site>" = "<sops-secret-name>"; };
```

- Non-clan machines: the string value is the sops secret name (as today).
- Clan machines: the string value is **ignored**; the clan generator name is
  derived from the site key.

Callers (`modules/nix-builder.nix`, `modules/machines/furfur.nix`) stay as-is.

### Three modules

**`nix` (shared, backend-neutral).** Declares the `accessTokens` option, an
internal option `_accessTokenSecretNames` (`site -> sops placeholder key`,
default `{}`), the base nix settings, the `nix-access-tokens` group, and the
single rendering template. The template reads the resolved names — the only
per-backend difference is which placeholder key each site maps to. The proven
pattern is `modules/clan/postgresql.nix`, where a clan-var secret is addressed as
`config.sops.placeholder."vars/<gen>/<file>"` (clan's sops backend registers
`config.sops.secrets."vars/<gen>/<file>"`).

```nix
names     = config.nixos-config.nix._accessTokenSecretNames;
tokenLine = lib.concatStringsSep " " (
  lib.mapAttrsToList (site: _: "${site}=${config.sops.placeholder.${names.${site}}}") cfg
);
# ... rendered under (lib.mkIf hasTokens ...):
sops.templates."nix-access-tokens" = {
  content = "extra-access-tokens = ${tokenLine}\n";
  group   = "nix-access-tokens";   # runner DynamicUser reads THIS rendered file
  mode    = "0440";
};
nix.extraOptions = ''!include ${config.sops.templates."nix-access-tokens".path}'';
```

sops-nix renders the placeholder (clan-var or plain sops) into the tmpfs template
as root; the rendered file is group-readable, so the `forgejo-runner` DynamicUser
(member of `nix-access-tokens`) can read it during flake-input fetches. This is
the exact chain that was failing.

**`nix-access-tokens-sops` (non-clan backend).** Resolves names to the attrset
values and declares the plain per-machine sops secrets:

```nix
config = {
  nixos-config.nix._accessTokenSecretNames = cfg;   # value = sops secret name
  sops.secrets = lib.mapAttrs' (_site: secretName: { name = secretName; value = {}; }) cfg;
};
```

**`nix-access-tokens-clan` (clan backend).** Resolves names to the clan-var
placeholder keys and declares one shared prompt generator per site (modeled on
`clan.core.vars.generators.tailscale-admin`):

```nix
genName = site: "nix-access-token-${lib.replaceStrings [ "." ] [ "-" ] site}";
config = {
  nixos-config.nix._accessTokenSecretNames = lib.mapAttrs (site: _: "vars/${genName site}/token") cfg;
  clan.core.vars.generators = lib.mapAttrs' (site: _: lib.nameValuePair (genName site) {
    share = true;
    prompts.token.description = "nix access token for ${site} (e.g. GitHub PAT)";
    files.token = { secret = true; deploy = true; };   # raw token stays root-only 0400
    script = ''tr -d '\n' < "$prompts/token" > "$out/token"'';
  }) cfg;
};
```

The raw token file is root-only; only root renders the template. No custom
owner/group/mode on the generator file is needed.

## Migration / operator steps

1. Land the `modules/nix.nix` change.
2. Run `clan vars generate` for `nix-builder-raum` / `nix-builder-valak`; when
   prompted, paste the **valid (Feb-2027) GitHub PAT**. Because the generator is
   `share = true`, the token is prompted once and shared to both builders.
3. Deploy both builders.
4. Remove the stale `extra-access-tokens:` block from
   `secrets/nix-builder-raum/secrets.yaml` and
   `secrets/nix-builder-valak/secrets.yaml`.
5. `furfur` is untouched.

## Verification

- On each builder, extract the rendered token and confirm auth:
  `curl -s -o /dev/null -w '%{http_code}\n' -H "Authorization: token <tok>" https://api.github.com/rate_limit` → **200**.
- Rendered file is `nix-access-tokens` group, mode `0440`.
- A forgejo runner job that fetches a github flake input succeeds (no rate-limit
  / auth error).

## Non-goals

- Migrating furfur (non-clan) to clan vars.
- Supporting sites other than `github.com` beyond what the generic attrset
  already allows (per-site generators compose, but only github.com is in use).
- Auto-generating the PAT (operator-supplied via prompt).

## Files touched

- `modules/nix.nix` — backend detection + clan-vars generator + shared template.
- `secrets/nix-builder-raum/secrets.yaml` — remove stale `extra-access-tokens`.
- `secrets/nix-builder-valak/secrets.yaml` — remove stale `extra-access-tokens`.
