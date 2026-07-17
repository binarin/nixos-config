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
- Allowlist is not a per-home runtime option (see Packaging).

## Architecture

Per app launch:

```
fuzzel / PATH / niri bind
      │   (.desktop Exec= and the PATH binary both point here)
      ▼
shitty-shit-run <app-key> -- <real binary> "$@"
      ├─ 1. select <app-key>'s allowlist (baked in from Nix)
      ├─ 2. fallback: if no user session bus / systemd-run unusable → exec "$@"
      ├─ 3. mktemp private socket:  $XDG_RUNTIME_DIR/shitty-shit/<app-key>.<pid>.bus
      ├─ 4. spawn per-launch  xdg-dbus-proxy $UPSTREAM <socket> --filter <allowlist>
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

One reusable script (a `writeShellApplication`), invoked as
`shitty-shit-run <app-key> -- <cmd> "$@"`. `<app-key>` selects the app's
allowlist. Sketch:

```sh
set -euo pipefail
app_key=${1:?app key required}; shift
[ "${1:-}" = "--" ] && shift

allow=()
case "$app_key" in
  chrome) allow=( @CHROME_ALLOW@ ) ;;   # substituted from Nix
  slack)  allow=( @SLACK_ALLOW@  ) ;;
  *) echo "shitty-shit-run: unknown app key '$app_key'" >&2; exit 64 ;;
esac

# Fallback: no usable user session → run directly, no isolation.
if ! systemctl --user show-environment >/dev/null 2>&1; then
  exec "$@"
fi

upstream=${DBUS_SESSION_BUS_ADDRESS:-unix:path=${XDG_RUNTIME_DIR:?}/bus}
dir="${XDG_RUNTIME_DIR:?}/shitty-shit"; mkdir -p "$dir"
sock="$dir/${app_key}.$$.bus"; rm -f "$sock"

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
- Socket uniqueness via `$$`.
- **Fallback**: if there is no usable user session bus, `exec "$@"` directly.
  The wrapped binaries are global (see Packaging), so the launcher must not
  assume a graphical session.

### 3. Per-app allowlist (module-level Nix data)

Structured, defined in the module, compiled into `shitty-shit-run`:

```nix
allowlist = {
  chrome = { talk = [ ]; own = [ ]; see = [ ]; };
  slack  = { talk = [ ]; own = [ ]; see = [ ]; };
};
```

- Each list renders to `--talk=<name>` / `--own=<name>` / `--see=<name>` flags
  for that app's `case` branch.
- **Starts empty**: with an empty policy, `xdg-dbus-proxy --filter` grants only
  the bus driver (`org.freedesktop.DBus`) and denies everything else —
  including `systemd1` (goal met). Names are added consciously as features are
  found broken. This is functionally identical to the validated
  `env -u DBUS_SESSION_BUS_ADDRESS` state, but with a growth path.
- Allowlist is **module-level Nix data, not a per-home option**: overlays are
  pure functions of `final`/`prev` and cannot read a home-manager option
  without extra machinery. Since the intent is "start empty, grow by editing,"
  editing the module and rebuilding is the natural workflow.

### 4. Chrome overlay integration

Extend the existing `modules/packages/google-chrome.nix` `chromeWrapper`. Today
it `exec`s `google-chrome-stable` (with optional `CHROME_PROXY`). Change the
final `exec` to route through the launcher:

```
exec ${shitty-shit-run}/bin/shitty-shit-run chrome -- <real google-chrome-stable> "$@"
```

keeping the existing `CHROME_PROXY` logic. The existing `.desktop`-rewrite
`sed` already points desktop entries at this wrapper, so no change there.

### 5. Slack overlay integration

Slack currently has only an icon-fixup overlay (`modules/programs/slack.nix`)
and no wrapper. Add the same shape as Chrome:

- A `slack` wrapper on `PATH` = `shitty-shit-run slack -- <real slack> "$@"`.
- A `.desktop` rewrite (same `sed`-over-`share/applications` trick as Chrome)
  so fuzzel routes through the wrapper.
- `symlinkJoin` the wrapper + rewritten desktop files ahead of upstream
  `slack`.

### 6. Packaging & gating

- **New module `modules/shitty-shit-slice.nix`** exposes
  `flake.overlays.shitty-shit-launcher` providing `final.shitty-shit-run`
  (built from the `allowlist` data), and a home option
  `programs.shitty-shit-slice.enable` that writes the slice unit file.
- Add `self.overlays.shitty-shit-launcher` to `defaultOverlays` in
  `modules/nix.nix` (alongside the existing `self.overlays.slack` and
  `self.overlays.my-google-chrome`, which are already applied globally at
  `modules/nix.nix:55,57`). Overlay order is irrelevant because the chrome/slack
  overlays reference `final.shitty-shit-run` (the composed fixpoint).
- The chrome & slack overlays reference `final.shitty-shit-run` by store path,
  so there is no `PATH` ambiguity for the wrapper's own call.
- `modules/gui.nix` enables `programs.shitty-shit-slice` when
  `programs.slack.enable || programs.google-chrome.enable`.

## Control / data flow

1. User launches Slack/Chrome (any path). The binary on `PATH` and the
   `.desktop` `Exec=` are both the overlay wrapper.
2. Wrapper calls `shitty-shit-run <app-key> -- <real binary>`.
3. Launcher spawns a per-instance `xdg-dbus-proxy` with the app's allowlist,
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
- Unknown `<app-key>` → exit 64 with a message.
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
  allowlists for isolation and a self-healing lifetime.
- **`env -u DBUS_SESSION_BUS_ADDRESS`** (no proxy): validated the mechanism but
  kills all session-bus features; it is the degenerate empty-allowlist case
  without a growth path.

## Files touched

- `modules/shitty-shit-slice.nix` (new): launcher overlay, `allowlist` data,
  `programs.shitty-shit-slice.enable` + slice unit file.
- `modules/nix.nix`: add `self.overlays.shitty-shit-launcher` to
  `defaultOverlays`.
- `modules/packages/google-chrome.nix`: route wrapper `exec` through
  `shitty-shit-run chrome`.
- `modules/programs/slack.nix`: add wrapper + `.desktop` rewrite routing
  through `shitty-shit-run slack`.
- `modules/gui.nix`: enable `programs.shitty-shit-slice` when Slack/Chrome
  enabled.
