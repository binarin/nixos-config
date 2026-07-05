# Reusable Forgejo runners — `forgejo-runner` clan service — design

## Problem

The nix-builder CI runners are locked to a single repository and use a runner
registration flow that Forgejo now deprecates.

Two coupled goals:

1. **Reuse** — the runners on `nix-builder-valak` (3) and `nix-builder-raum` (2)
   should serve *all* repositories owned by the `binarin` user, not just
   `nixos-config`. All target projects live under that one user.
2. **De-deprecate** — `modules/nix-builder.nix` uses `services.gitea-actions-runner`,
   whose registration is built on the **registration-token** flow. In
   forgejo-runner 12.12.0 both runner-side registration subcommands are marked
   deprecated:
   - `forgejo-runner register` (registration-token exchange)
   - `forgejo-runner create-runner-file` (shared-secret → `.runner` file)

   The non-deprecated runner path is `forgejo-runner daemon` with
   `--url --uuid --token-url --label` — no `.runner` file, no register step. The
   runner's permanent token is established **server-side** by
   `forgejo actions register --secret <40-hex> --scope <owner> …` (see
   `../forgejo/cmd/forgejo/actions.go` and `../forgejo/models/actions/forgejo.go`).

### Why scope is the lever for reuse

A runner's scope is fixed at registration: repo-level, org/user-level, or global
(`../forgejo/models/actions/runner_token.go`). The current runners are
repo-scoped to `nixos-config`. Registering them at **user scope**
(`--scope binarin`) makes every current and future repo under `binarin` able to
use them via the existing `native:host` label.

### The shared-secret pivot

`RegisterRunner` (`../forgejo/models/actions/forgejo.go`) derives the runner UUID
from the secret: `uuid = uuid(secret[:16] bytes)`, and the full 40-char hex
secret is the runner's permanent auth token. Registration is **idempotent keyed
by that UUID** — re-running with the same secret updates
name/owner/labels/ephemeral in place. Because the admin *chooses* the secret
(unlike a server-generated registration token), it can be a declarative
clan var defined before first use, and both sides derive the same identity with
no handshake:

- **Forgejo host** runs `forgejo actions register --secret <S> --scope binarin`.
- **Builder host** runs `forgejo-runner daemon --uuid uuid(S) --token <S>`.

One secret ⇒ one runner. Five runner instances ⇒ five distinct secrets.

## Approach

A new clan service module `flake.clan.modules.forgejo-runner` in
`modules/clan/forgejo-runner.nix`, `_class = "clan.service"`, modeled directly on
`modules/clan/postgresql.nix`. It has two roles plus a `perMachine` block.

| postgresql service | forgejo-runner service |
|---|---|
| `server` = PG host, provisions every client's role/db/password | **`server`** = forgejo host, *registers* every runner |
| `client` = consumer, declares `access.<label>` pairs | **`runner`** = builder host, declares how many runners it runs |
| `share=true` password generator per `(db,user)` | `share=true` secret generator per runner |

**Instance model:** the initial deployment uses a single instance named
`forgejo-runner` (scope `binarin`); the `forgejo` machine joins `server`, each
builder host joins `runner`. **Multiple instances co-exist** — see the
*Multiple instances* section — so runners for another owner are just a second
instance with its own `scope`.

Persistent (not ephemeral) runners — long-lived daemons with a warm `/nix/store`,
matching today's behavior. Ephemeral would re-register every job and lose cache
warmth, a poor fit for native host nix builders.

### Secret sharing (`perMachine`)

One `share = true` clan-vars generator per runner, declared in the module's
`perMachine` block (single code path, evaluated per machine) — the exact pattern
`postgresql.nix` uses. Generator script:

```
openssl rand -hex 20 | tr -d '\n' > $out/secret     # 40 hex chars = 20 bytes, no newline
```

Branch on `machine.roles`:

- **server machine** (`builtins.elem "server" machine.roles`) → declare the
  generator for *every* runner across all `runner`-role machines of *every*
  instance (the forgejo host must read them all to register them).
  `files.secret.restartUnits` = that runner's `forgejo-register-${ident}.service`.
- **runner machine** → declare only its own runners' generators.
  `files.secret.restartUnits` = that runner's `forgejo-runner-${ident}.service`.

Both `deploy = true`, `secret = true`, default `owner = root` / `mode = 0400`
(delivered to services via `LoadCredential`, so root-owned is fine). Referenced
by `config.clan.core.vars.generators.<gen>.files.secret.path` — the repo-wide
idiom.

### Naming (must be instance-namespaced)

To let multiple instances co-exist on the same machine, every generated
identifier is namespaced by `instanceName`. Define once:

- `ident = "${instanceName}-${machine}-${toString idx}"` — used for the
  generator name (`forgejo-runner-${ident}`), the systemd unit names
  (`forgejo-runner-${ident}.service`, `forgejo-register-${ident}.service`),
  `StateDirectory`, and the `DynamicUser` name.
- `displayName = if idx == 1 then machine else "${machine}-${toString idx}"` —
  the Forgejo `--name` only. It may repeat across instances (they are different
  owners in the UI); within one scope it is unique. The runner *identity* is the
  UUID derived from the (instance-namespaced, hence unique) secret, not the name.

### `runner` role (builder host)

**Interface (`settings`):**

- `count` : positive int, default `1` — runner instances on this host.
- `labels` : list of str, default `[ "native:host" ]`.
- `url` : str, default `"https://forgejo.lynx-lizard.ts.net"` — the forgejo
  instance. Held on the runner role so no cross-role read is needed.
- `hostPackages` : list of packages, default the current runner toolset (bash,
  coreutils, curl, git, nix, nodejs, podman, jq, just, s3cmd, niks3, …).
- `supplementaryGroups` : list of str, default `[ "podman" ]` — the builder host
  adds `"nix-access-tokens"`.

**`perInstance` `nixosModule`** — for each `idx` in `1..count`, emit
`forgejo-runner-${ident}.service`:

- `DynamicUser = true`, `User = "forgejo-runner"` (a fixed short name — `ident`
  would exceed the ~32-char username limit), `StateDirectory = "gitea-runner/${ident}"`
  (per-runner state isolation; warm caches persist across restarts; state lives
  under the existing `/var/lib/private/gitea-runner` mount).
- `SupplementaryGroups` = podman + `nix-access-tokens`; `DOCKER_HOST` →
  podman socket.
- `PATH` = `hostPackages`.
- `LoadCredential = "token:<generator secret path>"`.
- `ExecStart` = a wrapper script: read the secret from
  `$CREDENTIALS_DIRECTORY/token`, compute `uuid` from its first 16 bytes
  (`od`/`printf` → `8-4-4-4-12`), then
  `exec forgejo-runner daemon --url <url> --uuid "$uuid"
  --token-url "file:$CREDENTIALS_DIRECTORY/token" --label <l1> --label <l2> …
  --config <base.yaml>`.
- `base.yaml` — non-secret daemon config (capacity, host/container options),
  generated with the settings format. No token, no uuid in it.
- `Restart = "on-failure"`, `wantedBy = [ "multi-user.target" ]`, wants
  network-online + podman.

The role also enables `virtualisation.podman.enable = true` (idempotent) — the
runner runtime is owned by the clan service.

### `server` role (forgejo host)

**Interface (`settings`):**

- `scope` : str, default `"binarin"` — owner scope passed to `register`
  (`""` = global).

**`perInstance` `nixosModule`** — flatten `roles.runner.machines` into the full
runner list (`ident`, `displayName`, labels, secret generator), exactly like
`postgresql.nix` flattens `roles.client.machines`. For each runner emit
`forgejo-register-${ident}.service`:

- `Type = "oneshot"`, `RemainAfterExit = true`, `wantedBy = multi-user.target`.
- `requires`/`after` = `forgejo.service` (schema exists once the server has
  migrated).
- `Restart = "on-failure"` + `RestartSec` — forgejo uses **sqlite** (no
  `database` set → NixOS default `sqlite3`), so the register CLI writing while the
  server holds the DB can hit a brief write lock; retry rides it out.
- Script runs as the forgejo user, replicating the `forgejo-cli` env
  (`FORGEJO_WORK_DIR = stateDir`, `FORGEJO_CUSTOM = customDir`, `runuser -u <user>`),
  invoking:
  `forgejo actions register --secret-file <generator secret path>
  --scope <scope> --name <displayName> --labels <csv>`.
  `--secret-file` keeps the secret out of argv.
- Re-runs on rebuild and on secret rotation (via the generator's `restartUnits`).

**Serialize registration (sqlite single-writer).** All `forgejo-register-*` units
on the forgejo host — *across every instance* — must run one at a time, or
concurrent writers race on the same sqlite DB (`database is locked`). This is the
same hazard and remedy as the postgres module's provisioning serialization
(`c26706fd`): chain the register units in a stable order via `after` +
`before`/predecessor edges so each starts only after the previous finishes. Since
serialization must span instances, the ordering is best expressed where all
instances are visible — the `perMachine` block on the server machine (which
iterates all `instances`) computes the global register-unit order and emits the
`after` edges. Builders' daemons are unaffected; only server-side registration
serializes.

### Machine wiring (mirrors postgres/metabase split)

```nix
# modules/machines/forgejo.nix
clan.inventory.instances.forgejo-runner = {
  module = { input = "self"; name = "forgejo-runner"; };
  roles.server.machines.forgejo.settings.scope = "binarin";
};

# modules/machines/nix-builder-valak.nix
clan.inventory.instances.forgejo-runner.roles.runner.machines.nix-builder-valak.settings.count = 3;

# modules/machines/nix-builder-raum.nix
clan.inventory.instances.forgejo-runner.roles.runner.machines.nix-builder-raum.settings.count = 2;
```

### Multiple instances

Runners for a **different owner** are a second instance of the same service; the
forgejo host joins `server` in each. Because `scope` is a `server`-role setting,
one instance = one scope.

```nix
clan.inventory.instances.forgejo-runner-acme = {
  module = { input = "self"; name = "forgejo-runner"; };
  roles.server.machines.forgejo.settings.scope = "acme";
  roles.runner.machines.nix-builder-valak.settings.count = 2;   # same host, different owner — OK
};
```

This works because:

- **`perMachine` iterates all instances** (the `postgresql.nix` pattern), so the
  forgejo host declares generators + register units for every instance, and the
  cross-instance register serialization is computed there.
- **All internal identifiers are `instanceName`-namespaced** (`ident`), so a host
  running runners for two owners has no unit/`StateDirectory`/user collisions.
- Secrets are per-`ident` and thus per-instance; distinct secrets ⇒ distinct
  UUIDs ⇒ distinct Forgejo runners, even when the display name repeats across
  owners.

A single instance spanning *multiple* scopes is intentionally not supported — put
each scope in its own instance.

### `modules/nix-builder.nix` slims down

Remove the runner-provisioning parts now owned by the clan service:

- `services.gitea-actions-runner` block and its `instances`.
- `nixos-config.nix-builder.runnerCount` option (replaced by the clan `count`
  setting).
- sops `nixos-config-runner-token` secret, its env template, and the
  `serviceEscapedName` restart wiring.
- The `nix-builder.runnerCount = N` lines in the two builder machine files.

Keep the pure **host** concerns: the srvos `nix-remote-builder` role,
`nix.settings.system-features`/`build-dir`, `nixos-config.nix.accessTokens`
(github token + the `nix-access-tokens` group the daemons join), podman is now
enabled by the clan runner role, and `export-metrics`.

## Consequences / out of scope

- **No pruning.** `register` only upserts; reducing `count` or removing a builder
  leaves stale runner rows in Forgejo. Cleanup is a manual
  `forgejo-cli actions …` step. Not automated here.
- **Migration.** After rollout, retire the old repo-scoped
  `nixos-config-runner-token` secret and the old `nixos-config*` runner rows
  (manual, one-time).
- **`--scope binarin` correctness.** Confirm the exact forgejo owner/username is
  `binarin` before first deploy.
- **`forgejo-runner` version.** Pinned via `pkgs.forgejo-runner` (12.12.0 at
  design time); the `daemon --uuid/--token-url` interface is the target.

## Verification

- `nix flake check` / evaluate `nixosConfigurations.{forgejo,nix-builder-valak,nix-builder-raum}`.
- Deploy; confirm five `forgejo-register-*.service` units succeed on the forgejo
  host and five runners appear under the `binarin` user in Forgejo, each showing
  the `native:host` label and `idle`/`online`.
- Confirm five `forgejo-runner-*.service` daemons are active across the two
  builders with no `register`/`create-runner-file` invocation in their logs.
- From a *second* repo under `binarin`, run a workflow with `runs-on: native`
  and confirm it is picked up and completes.
- Rebuild again and confirm the register oneshots are idempotent (runners not
  duplicated).
