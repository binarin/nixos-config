# shitty-shit.slice — memory-capped slice for Slack + Chrome

Date: 2026-07-17
Status: Approved (design)

## Problem

Slack and Google Chrome should live in a dedicated systemd **user** slice,
`shitty-shit.slice`, with a shared memory budget: `MemoryHigh=8G` (soft),
`MemoryMax=10G` (hard), and swap disabled. The limit is shared across
everything in the slice.

The obstacle: Chromium and Electron apps escape whatever slice they are
launched in. On startup they call `org.freedesktop.systemd1.Manager.StartTransientUnit`
over D-Bus to create their **own** dynamically-named `app-*.scope`, which lands
under `app.slice`. Because the escape unit is dynamically named, the static
drop-in trick used elsewhere in this repo (`corporate-bloat.slice` in
`modules/machines/murmur.nix`, which pins fixed unit names via
`Slice=` drop-ins) does not apply — there is no fixed unit name to attach to.

Upstream bug: <https://issues.chromium.org/issues/437667316> — *"Detect if
being started in a systemd unit and do not create own scope."* Until that
lands, denying the app the ability to make the call is the workaround.

## Key insight (empirically validated)

The escape call travels over the **session bus** (`DBUS_SESSION_BUS_ADDRESS`),
not the private `$XDG_RUNTIME_DIR/systemd/private` socket. Confirmed by hand:

```
systemd-run --user --slice=shitty-shit.slice --scope --collect --same-dir \
  -- env -u DBUS_SESSION_BUS_ADDRESS slack
```

kept Slack inside the slice — removing the session bus removed its ability to
reach `systemd1`, so it could not escape.

Therefore: if we deny the app access to `org.freedesktop.systemd1` on the
session bus while preserving the rest of the bus, `StartTransientUnit` fails,
the app cannot create its escape scope, and its child processes (renderers,
GPU, utility) simply **inherit the cgroup** of wherever we launched it. We only
have to place the *top-level* process into the slice; the dynamic-unit problem
disappears instead of being chased.

`env -u DBUS_SESSION_BUS_ADDRESS` is the crude form of this — it also kills
notifications, portals, tray, secrets, etc. A filtering D-Bus proxy is the
refined form: deny only `systemd1`, keep everything else.

## Goals

- `shitty-shit.slice` as a user slice with `MemoryHigh=8G`, `MemoryMax=10G`,
  `MemorySwapMax=0`.
- Slack and Google Chrome always launch into that slice, regardless of launch
  path (fuzzel/`.desktop`, `PATH`, niri keybind).
- Chromium/Electron cannot escape the slice.
- Non-`systemd1` D-Bus functionality preserved, via a per-app allowlist that
  starts empty and grows as features are found to break.

## Non-goals

- Not a general sandbox. The proxy blocks exactly one thing (`systemd1`); the
  allowlist exists only to re-permit legitimate session-bus use.
- No CPU/IO limits (unlike `corporate-bloat.slice`).
- Whitelist is not a per-home runtime option — it is Nix data at the
  `wrapShittyShit` call site (see §3).

## Architecture

Per app launch:

```
fuzzel / PATH / niri bind
      │   (every wrapped bin/* and rewritten .desktop Exec= point here)
      ▼
shitty-shit-run <proxy filter flags…> -- <real binary> "$@"
      ├─ 1. flags before `--` are this app's whitelist (baked in by wrapShittyShit)
      ├─ 2. fallback: if no user session bus / systemd-run unusable → exec "$@"
      ├─ 3. mktemp private socket:  $XDG_RUNTIME_DIR/shitty-shit/<name>.<pid>.bus
      ├─ 4. spawn per-launch  xdg-dbus-proxy $UPSTREAM <socket> --filter <flags>
      │        (NO --talk for org.freedesktop.systemd1 → StartTransientUnit denied)
      ├─ 5. DBUS_SESSION_BUS_ADDRESS=unix:path=<socket> \
      │        systemd-run --user --scope --collect --same-dir \
      │          --slice=shitty-shit.slice -- <real binary> "$@"
      └─ 6. on exit: trap → kill proxy, rm socket
```

## Components

### 1. Slice unit

Raw user-slice unit, dropped via home-manager (home-manager has no
`systemd.user.slices` option, so we write the unit file directly — the repo
already uses `xdg.configFile` and `systemd.user.services`, e.g.
`modules/hyprland.nix`):

```nix
xdg.configFile."systemd/user/shitty-shit.slice".text = ''
  [Unit]
  Description=Memory-capped slice for Slack + Chrome

  [Slice]
  MemoryHigh=8G
  MemoryMax=10G
  MemorySwapMax=0
'';
```

- User slice under `user@.service` — the apps are session processes.
- `MemoryHigh` = soft (reclaim pressure / throttle), `MemoryMax` = hard
  (OOM-kill within the slice), `MemorySwapMax=0` disables swap for the slice.
- All three apply to the slice total, shared across every process in it.

### 2. Launcher `shitty-shit-run`

One reusable, **app-agnostic** script (a `writeShellApplication`), invoked as
`shitty-shit-run <proxy filter flags…> -- <cmd> "$@"`. Everything before `--`
is the whitelist for this app, passed in by `wrapShittyShit` at wrap time —
the launcher has no per-app knowledge. Sketch:

```sh
set -euo pipefail

# Everything before `--` is this app's xdg-dbus-proxy whitelist, baked in
# per-package by wrapShittyShit. systemd1 is denied simply by never being
# granted → Chromium/Electron cannot StartTransientUnit, so cannot escape.
# Upstream: https://issues.chromium.org/issues/437667316
#   "Detect if being started in a systemd unit and do not create own scope."
allow=()
while [ "$#" -gt 0 ] && [ "${1:-}" != "--" ]; do allow+=( "$1" ); shift; done
[ "${1:-}" = "--" ] && shift
[ "$#" -gt 0 ] || { echo "shitty-shit-run: no command" >&2; exit 64; }

# Fallback: no usable user session → run directly, no isolation.
if ! systemctl --user show-environment >/dev/null 2>&1; then
  exec "$@"
fi

name=$(basename "$1")
upstream=${DBUS_SESSION_BUS_ADDRESS:-unix:path=${XDG_RUNTIME_DIR:?}/bus}
dir="${XDG_RUNTIME_DIR:?}/shitty-shit"; mkdir -p "$dir"
sock="$dir/${name}.$$.bus"; rm -f "$sock"

xdg-dbus-proxy "$upstream" "$sock" --filter "${allow[@]}" &
proxy=$!
trap 'kill "$proxy" 2>/dev/null || true; rm -f "$sock"' EXIT

for _ in $(seq 1 50); do [ -S "$sock" ] && break; sleep 0.05; done

rc=0
DBUS_SESSION_BUS_ADDRESS="unix:path=$sock" \
  systemd-run --user --scope --collect --same-dir \
    --slice=shitty-shit.slice -- "$@" || rc=$?
exit $rc
```

Design points:
- **Per-launch proxy**: one `xdg-dbus-proxy` per invocation, torn down by the
  `trap` when the app exits. The proxy runs in the wrapper's own cgroup
  (outside the slice) — trivial memory, and it keeps the 8/10G budget for the
  apps themselves.
- Do **not** `exec` the `systemd-run` — the shell must survive to run the
  cleanup `trap`. `--scope` runs synchronously, so the script blocks until the
  app exits, then cleans up.
- `--collect` GCs the transient scope unit on exit; `--same-dir` preserves CWD.
- Socket uniqueness via `$$`; socket name prefixed with the command basename
  for readability.
- **Fallback**: if there is no usable user session bus, `exec "$@"` directly.
  The wrapped binaries are global (see Packaging), so the launcher must not
  assume a graphical session.

### 3. `wrapShittyShit` — the wrapper function

The whitelist is **not** central data and there is **no** app-key registry.
Instead a single Nix function wraps any package:

```nix
wrapShittyShit = pkg: { talk ? [ ], own ? [ ], see ? [ ] }: <derivation>
```

It returns a package where:
- every `bin/*` executable is replaced by a small wrapper that runs
  `shitty-shit-run <flags> -- <pkg>/bin/<exe> "$@"`, with `<flags>` rendered
  from `talk`/`own`/`see` (`--talk=<name>` / `--own=<name>` / `--see=<name>`);
- every `.desktop` `Exec=` is rewritten to the wrapped `bin/*` (so fuzzel and
  other `.desktop` launches route through the wrapper too);
- all other outputs (icons, resources, libs) pass through unchanged, and
  `meta.mainProgram` is preserved.

Likely implementation: a `symlinkJoin` (or `runCommand`) that places the
generated wrappers and rewritten desktop files **ahead of** the original
package so they win the name collision; wrappers reference the *original*
store path so there is no recursion.

Usage (whitelist lives at the call site, starts empty):

```nix
wrapShittyShit prev.slack { talk = [ ]; own = [ ]; see = [ ]; }
```

- **Starts empty**: with an empty policy, `xdg-dbus-proxy --filter` grants only
  the bus driver (`org.freedesktop.DBus`) and denies everything else —
  including `systemd1` (goal met). Names are added consciously as features are
  found broken. Functionally identical to the validated
  `env -u DBUS_SESSION_BUS_ADDRESS` state, but with a growth path.
- **Whitelist is Nix data at the wrap site, not a per-home runtime option** —
  overlays are pure functions of `final`/`prev`. "Start empty, grow by editing
  the call site" is the intended workflow, and this keeps the moving Nix parts
  minimal (one function, no central table, reusable for any future app).

### 4. Chrome integration

Keep the existing `CHROME_PROXY` wrapper in
`modules/packages/google-chrome.nix`, then wrap its result:

```nix
google-chrome = final.wrapShittyShit chromeWithProxy {
  talk = [ ]; own = [ ]; see = [ ];
};
```

Composition keeps the two concerns separate (proxy env vs. slice/bus
isolation). Because `wrapShittyShit` rewrites every `bin/*` and `.desktop` in
the package it is handed, the bespoke desktop-rewrite `sed` in the current
chrome overlay is no longer needed.

### 5. Slack integration

Keep the icon fixup in `modules/programs/slack.nix`, then wrap:

```nix
slack = final.wrapShittyShit slackWithIcons {
  talk = [ ]; own = [ ]; see = [ ];
};
```

No hand-rolled wrapper or desktop `sed` — `wrapShittyShit` handles both.

### 6. Packaging & gating

- **New module `modules/shitty-shit-slice.nix`** exposes
  `flake.overlays.shitty-shit-launcher`, which adds two things to `pkgs`:
  - `final.shitty-shit-run` — the app-agnostic runtime launcher script.
  - `final.wrapShittyShit` — the `pkg: { talk, own, see }: derivation` function
    from §3; its generated wrappers reference `final.shitty-shit-run` by store
    path (no `PATH` ambiguity).

  The module also defines the home option `programs.shitty-shit-slice.enable`
  that writes the slice unit file.
- Add `self.overlays.shitty-shit-launcher` to `defaultOverlays` in
  `modules/nix.nix` (alongside the existing `self.overlays.slack` and
  `self.overlays.my-google-chrome`, already applied globally at
  `modules/nix.nix:55,57`). Overlay order is irrelevant — the chrome/slack
  overlays reference `final.wrapShittyShit` (the composed fixpoint).
- The chrome & slack overlays call `final.wrapShittyShit`; the whitelist lives
  at those call sites.
- `modules/gui.nix` enables `programs.shitty-shit-slice` when
  `programs.slack.enable || programs.google-chrome.enable`.

## Control / data flow

1. User launches Slack/Chrome (any path). The wrapped `bin/*` on `PATH` and the
   rewritten `.desktop` `Exec=` both point at the generated wrapper.
2. Wrapper calls `shitty-shit-run <whitelist flags> -- <real binary>`.
3. Launcher spawns a per-instance `xdg-dbus-proxy` with that whitelist,
   exposing a filtered socket.
4. Launcher runs the real binary via `systemd-run --user --scope --collect
   --same-dir --slice=shitty-shit.slice`, with `DBUS_SESSION_BUS_ADDRESS`
   pointed at the filtered socket.
5. The app tries `StartTransientUnit` → denied → stays in `shitty-shit.slice`;
   children inherit the cgroup.
6. App exits → scope GC'd by `--collect`; `trap` kills the proxy and removes
   the socket.

## Error handling

- No usable user session bus → launcher `exec`s the app directly (no slice, no
  proxy) rather than failing to start it.
- No command after `--` → exit 64 with a message.
- Proxy socket never appears within the bounded wait → the `systemd-run` step
  still runs; the app gets a dead bus address and behaves as with an empty
  allowlist (degraded, not crashed). (Implementation may harden this.)

## Verification plan

On a GUI machine, after implementation:

1. Launch Chrome via the wrapper; `systemd-cgls --user` → Chrome process tree
   sits under `shitty-shit.slice`, and **no** `app-*.scope` appears under
   `app.slice`.
2. Repeat for Slack.
3. `systemctl --user show shitty-shit.slice -p MemoryHigh -p MemoryMax
   -p MemorySwapMax` → `8G / 10G / 0`.
4. With the empty allowlist, expect degraded IPC (no notifications/portals) —
   confirmation the filter is live and `systemd1` denied. Grow allowlists from
   there.
5. Close app → its `xdg-dbus-proxy` is gone and the socket removed.

Step 2 is already partially validated by hand via
`env -u DBUS_SESSION_BUS_ADDRESS slack` (Slack stayed in the slice).

## Risks & contingencies

- **Chrome still escapes with an empty allowlist** → it would be using the
  private `$XDG_RUNTIME_DIR/systemd/private` socket instead of the session bus.
  Contingency: also hide that socket from the app (namespace / `bwrap`).
  Considered unlikely given the Slack result; documented, not built upfront.
- **Singleton behavior**: if an instance launched outside the wrapper is
  already running, a new wrapped launch forwards to it and exits (its
  short-lived proxy tears down); placement follows the *first* instance.
  Mitigated because the wrapper is the entrypoint on every path.
- **Empty allowlist intentionally breaks** notifications/tray/portals until
  grown — expected, not a bug.

## Alternatives considered

- **Static `Slice=` drop-in** (like `corporate-bloat.slice`): impossible — the
  escape unit is dynamically named.
- **One long-running shared proxy** (single filtered socket for both apps):
  simpler wiring, but rejected in favor of per-launch proxies with per-app
  whitelists for isolation and a self-healing lifetime.
- **Per-app-key launcher with a central allowlist table** (`shitty-shit-run
  chrome`, a `case` in the script): rejected in favor of a single
  `wrapShittyShit pkg { … }` function that takes the whitelist at the call
  site — app-agnostic launcher, no registry, reusable for any package, fewer
  moving Nix parts.
- **`env -u DBUS_SESSION_BUS_ADDRESS`** (no proxy): validated the mechanism but
  kills all session-bus features; it is the degenerate empty-whitelist case
  without a growth path.

## Files touched

- `modules/shitty-shit-slice.nix` (new): `shitty-shit-run` + `wrapShittyShit`
  overlay, `programs.shitty-shit-slice.enable` + slice unit file.
- `modules/nix.nix`: add `self.overlays.shitty-shit-launcher` to
  `defaultOverlays`.
- `modules/packages/google-chrome.nix`: wrap the `CHROME_PROXY` variant with
  `final.wrapShittyShit` (drop the bespoke desktop `sed`).
- `modules/programs/slack.nix`: wrap the icon-fixup variant with
  `final.wrapShittyShit`.
- `modules/gui.nix`: enable `programs.shitty-shit-slice` when Slack/Chrome
  enabled.
