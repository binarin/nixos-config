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

All changes are in `modules/nix.nix` (the `flake.nixosModules.nix` module),
plus removal of the now-stale sops entries.

### Backend detection

Add `specialArgs` to the module arguments and compute:

```nix
isClan = specialArgs ? clan-core;
```

This is the established repo idiom (`modules/impermanence.nix` uses
`specialArgs ? clan-core`).

### Interface — unchanged

```nix
nixos-config.nix.accessTokens = { "<site>" = "<sops-secret-name>"; };
```

- Non-clan machines: the string value is the sops secret name (as today).
- Clan machines: the string value is **ignored**; the clan generator name is
  derived from the site key.

Callers (`modules/nix-builder.nix`, `modules/machines/furfur.nix`) stay as-is.

### Shared rendering; only the secret name differs

The proven pattern is `modules/clan/postgresql.nix`, where a clan-var secret is
addressed in a sops template as `config.sops.placeholder."vars/<gen>/<file>"`
(clan's sops backend registers `config.sops.secrets."vars/<gen>/<file>"`).

```nix
genName       = site: "nix-access-token-${lib.replaceStrings [ "." ] [ "-" ] site}";
# e.g. "github.com" -> "nix-access-token-github-com"
secretNameFor = site: if isClan then "vars/${genName site}/token" else cfg.${site};
tokenLine     = lib.concatStringsSep " " (
  lib.mapAttrsToList (site: _: "${site}=${config.sops.placeholder.${secretNameFor site}}") cfg
);
```

### Clan path (`isClan`) — shared prompt generator per site

Modeled on `clan.core.vars.generators.tailscale-admin` (prompt + `share = true`):

```nix
clan.core.vars.generators = lib.mapAttrs' (site: _: lib.nameValuePair (genName site) {
  share = true;
  prompts.token.description = "nix access token for ${site} (e.g. GitHub PAT)";
  files.token = { secret = true; deploy = true; };   # raw token stays root-only 0400
  script = ''tr -d '\n' < "$prompts/token" > "$out/token"'';
}) cfg;
```

The raw token file is root-only; only root renders the sops template, so no
custom owner/group/mode is needed on the generator file.

### Non-clan path (furfur) — unchanged

```nix
sops.secrets = lib.mapAttrs' (_site: secretName: {
  name = secretName;
  value = { };
}) cfg;
```

### Shared template + include (both backends) — unchanged from today

This is where group-readability lives (the link that was failing):

```nix
sops.templates."nix-access-tokens" = {
  content = "extra-access-tokens = ${tokenLine}\n";
  group   = "nix-access-tokens";
  mode    = "0440";
};
nix.extraOptions = ''!include ${config.sops.templates."nix-access-tokens".path}'';
```

sops-nix substitutes the placeholder (clan-var or plain sops) into a tmpfs file
as root; the rendered file is group-readable by `nix-access-tokens`, which the
runner DynamicUser belongs to.

### Config structure (`modules/nix.nix`)

```nix
config = lib.mkMerge [
  { /* base nix settings, users.groups.nix-access-tokens, trusted-users */ }

  (lib.mkIf (hasTokens && !isClan) {
    sops.secrets = /* per-site from attrset value */;
  })

  (lib.mkIf (hasTokens && isClan) {
    clan.core.vars.generators = /* per-site shared prompt generators */;
  })

  (lib.mkIf hasTokens {
    sops.templates."nix-access-tokens" = { /* ... */ };
    nix.extraOptions = ''!include ${config.sops.templates."nix-access-tokens".path}'';
  })
];
```

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
