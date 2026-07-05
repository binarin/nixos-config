# forgejo-runner: regular users instead of DynamicUser + env hacks

**Date:** 2026-07-05
**Status:** approved (design)
**Touches:** `modules/clan/forgejo-runner.nix` (runner role only)

## Problem

The `forgejo-runner` clan service runs each daemon with:

```nix
serviceConfig = {
  DynamicUser = true;
  User = "forgejo-runner";
  StateDirectory = "gitea-runner/${ident}";
  ...
};
environment = {
  DOCKER_HOST = "unix:///run/podman/podman.sock";
  HOME = "/var/lib/gitea-runner/${ident}";
  XDG_CACHE_HOME = "/var/lib/gitea-runner/${ident}/.cache";
  XDG_CONFIG_HOME = "/var/lib/gitea-runner/${ident}/.config";
};
```

With `DynamicUser = true`, systemd gives the daemon `HOME=/` (there is no passwd
home for the transient user). Jobs inherit the daemon environment, so tools
(`nix`, `git`) try to write `~/.cache` on the read-only `/`, failing every job.
The last two commits (`519025e7`, `8f6f5f20`) worked around this by hand-setting
`HOME` and the two `XDG_*` bases. That is three environment overrides
compensating for the absence of a real user.

## Goal

Replace `DynamicUser` with a **real, declared user per clan-service instance** so
systemd populates `HOME`/`USER`/`LOGNAME`/`SHELL` from `/etc/passwd`
automatically, and delete the three manual env overrides. The user has a real,
persistent, correctly-owned home directory; `HOME`/`XDG_*` "just work".

Non-goal: this does **not** reach tools run inside Bazel action sandboxes — those
still need `--action_env`/in-`cmd` `HOME` in the repo (see the finance-take-2
note). Out of scope here.

## Decisions (from brainstorming)

1. **Granularity: one user per clan-service instance.** The user identity is
   keyed on `instanceName` (currently `forgejo-runner`). Every daemon belonging
   to that instance on a given host shares that one user and one home. A second
   instance on the same host gets its own user (`instanceName` is unique per
   instance, so no collision). `forgejo-runner` is well under the ~32-char
   username limit — no name shortening needed.

2. **State layout: home is the state root.** User home is
   `/var/lib/${instanceName}` (`/var/lib/forgejo-runner`). Each daemon works in a
   per-runner subdirectory under the home; `HOME` and the job working directories
   share one owned tree; `~/.cache` and `~/.config` are shared across the host's
   daemons.

3. **UID/GID: auto system UID.** `isSystemUser = true` with no fixed UID. NixOS
   stably allocates and records a system UID per host. This keeps the clan module
   fully generic (no coupling to this repo's `inventory/users-groups.nix`). Safe
   here because the home is host-local — no shared NFS/volume permissions depend
   on a specific number.

4. **Shell: `pkgs.bashInteractive`.** A "regular user" whose `$SHELL` is usable
   by tools that consult it (e.g. `nix`). Harmless because daemons are launched by
   systemd, not via login.

5. **DynamicUser hardening: dropped, not replaced.** Removing `DynamicUser` also
   removes its implicit `PrivateTmp` / `ProtectSystem=strict` / `ProtectHome` /
   `RemoveIPC` defaults. Jobs already require broad host access (podman socket,
   nix daemon), so effective behavior is unchanged. Flagged here deliberately; we
   do **not** re-add hardening in this change to keep scope on the HOME/user
   cleanup.

## Changes

All changes are in the `runner` role's `perInstance.nixosModule` in
`modules/clan/forgejo-runner.nix`. The `server` role and the secret/serialization
`perMachine` logic are untouched.

Let `sub = r.displayName` (e.g. `nix-builder-valak`, `nix-builder-valak-2`,
`nix-builder-valak-3`) — the per-daemon disambiguator within a host+instance.

### 1. Declare the user/group once (outside the daemon loop)

```nix
users.groups.${instanceName} = { };
users.users.${instanceName} = {
  isSystemUser = true;
  group = instanceName;
  home = "/var/lib/${instanceName}";
  createHome = true;
  shell = pkgs.bashInteractive;
  description = "Forgejo Actions runner (${instanceName})";
};

systemd.tmpfiles.rules = [
  "d /var/lib/${instanceName} 0750 ${instanceName} ${instanceName} -"
];
```

The tmpfiles rule guarantees the shared home exists and is owned regardless of
which daemon starts first (belt-and-suspenders with `createHome`).

### 2. Per-daemon service (`mkDaemon`)

- `serviceConfig`: remove `DynamicUser`; set
  - `User = instanceName;`
  - `Group = instanceName;`
  - `StateDirectory = "${instanceName}/${sub}";` (systemd creates + chowns the
    per-daemon subdir under the shared home)
  - `WorkingDirectory = "/var/lib/${instanceName}/${sub}";`
- `baseConfig`: `runner.file = "/var/lib/${instanceName}/${sub}/.runner";`
- `environment`: keep **only** `DOCKER_HOST`. Delete `HOME`, `XDG_CACHE_HOME`,
  `XDG_CONFIG_HOME`. systemd sets `HOME=/var/lib/forgejo-runner` from passwd;
  unset XDG bases correctly default to `$HOME/.cache` / `$HOME/.config`.
- `SupplementaryGroups` (podman, nix-access-tokens), `LoadCredential`,
  `ExecStart`, `Restart`/`RestartSec`, ordering, and `path` are unchanged.

### Resulting on-disk layout

```
/var/lib/forgejo-runner/                 owner forgejo-runner:forgejo-runner
  ├── .cache/  .config/                  shared across the host's daemons
  ├── nix-builder-valak/.runner   + work   (idx 1 → bare machine name)
  ├── nix-builder-valak-2/.runner + work
  └── nix-builder-valak-3/.runner + work
```

## Migration

Low-risk, no data move:

- The old state at `/var/lib/gitea-runner/${ident}` was owned by the transient
  DynamicUser UID. The `.runner` file is disposable: registration is idempotent
  (the runner UUID is derived from the shared secret, not from any file), so each
  daemon recreates `.runner` at its new path on first connect. **No
  re-registration, no manual copy.**
- Old `/var/lib/gitea-runner/*` directories become orphaned. They are safe to
  delete manually as a one-off; this change does **not** auto-`rm` any state.

## Edge cases / risks

- **Home ownership before first daemon start.** Covered by `createHome = true`
  plus the tmpfiles rule; `StateDirectory` also chowns the per-daemon subdirs.
- **Shared caches under concurrency.** Multiple daemons (capacity 1 each) share
  `~/.cache`. This is the accepted layout (decision 2); nix/git caches are
  concurrency-tolerant. If contention ever bites, the fix is a per-`sub`
  `XDG_CACHE_HOME` — explicitly out of scope now.
- **Second instance on one host.** `users.users.${instanceName}` /
  `users.groups.${instanceName}` are keyed by instance name, so two instances
  produce two distinct users with no unit/dir/uid collision.
- **Lost DynamicUser hardening.** See decision 5 — intentional, behavior
  unchanged in practice.

## Verification

- `nix flake check`, and eval of the `nix-builder-valak` and `nix-builder-raum`
  nixosConfigurations.
- After deploy on a runner host:
  - `systemctl show -p User -p Group -p Environment forgejo-runner-<ident>.service`
    shows `User=forgejo-runner` and `HOME=/var/lib/forgejo-runner`, with no
    `XDG_*` overrides.
  - `getent passwd forgejo-runner` shows the home and shell.
- Run one real CI job (the original failure mode): confirm nix/git write under
  `/var/lib/forgejo-runner/.cache` and the job succeeds.
