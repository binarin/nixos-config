# shitty-shit.slice Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Confine Slack and Google Chrome to a memory-capped systemd **user**
slice (`shitty-shit.slice`, 8G soft / 10G hard / no swap), preventing
Chromium/Electron from escaping it via their own `StartTransientUnit` scope.

**Architecture:** A per-launch `xdg-dbus-proxy` denies `org.freedesktop.systemd1`
on the session bus while passing a per-app whitelist; the app is launched into
the slice with `systemd-run --user --scope`, and only the app (not
`systemd-run` itself) receives the filtered bus. A single `wrapShittyShit`
Nix function rewrites every `bin/*` and `.desktop` of any package to route
through the launcher.

**Tech Stack:** Nix (flake-parts, dendritic auto-imported modules), Nix
overlays, home-manager, bash (`writeShellApplication`), `xdg-dbus-proxy`,
systemd user manager.

## Global Constraints

- Slice limits, exact values: `MemoryHigh=8G`, `MemoryMax=10G`,
  `MemorySwapMax=0`. It is a **user** slice under `user@.service`.
- Whitelists start **empty** (`talk = []; own = []; see = [];`) — grown later by
  editing the call site. Empty is intentional; expect notifications/tray/portals
  to be dead until grown.
- The launcher must deny `org.freedesktop.systemd1` and **must not** route
  `systemd-run --user`'s own D-Bus traffic through the proxy (it needs the real
  `systemd1` to create the scope). Only the launched app gets the proxy socket,
  via `env` inside the scope.
- Follow existing repo conventions: overlays via `flake.overlays.<name>` (see
  `modules/programs/slack.nix`, `modules/packages/google-chrome.nix`); home
  modules via `flake.homeModules.<name>` with a `key = "nixos-config.modules.home.<name>";`
  attribute (see `modules/firefox.nix`, `modules/desktop-essentials.nix`);
  repo file contents read with `self.lib.self.read "bin/<name>"` (reads
  `files/<name>`).
- Upstream bug this works around:
  <https://issues.chromium.org/issues/437667316> — "Detect if being started in
  a systemd unit and do not create own scope."
- Spec: `docs/superpowers/specs/2026-07-17-shitty-shit-slice-design.md`.
- Verification build form (perSystem `pkgs` carries all default overlays):
  `nix build '.#configured-pkgs.x86_64-linux.nixpkgs.<attr>'`.
- **Deploy/runtime is operator-run.** These tasks stop at build/eval
  verification; the final cgroup-placement test (Task 6) is executed by the
  operator on a GUI machine.

---

## File Structure

- `files/bin/shitty-shit-run` (new): the launcher script body (no shebang / no
  `set` line — `writeShellApplication` adds those). One responsibility: given a
  whitelist and a command, run the command in `shitty-shit.slice` on a
  systemd1-denied filtered bus.
- `modules/shitty-shit-slice.nix` (new): one dendritic module exposing
  - `flake.overlays.shitty-shit-launcher` → adds `pkgs.shitty-shit-run` and
    `pkgs.wrapShittyShit`;
  - `flake.homeModules.shitty-shit-slice` → the
    `programs.shitty-shit-slice.enable` option + the slice unit file.
- `modules/nix.nix` (modify): add `self.overlays.shitty-shit-launcher` to
  `defaultOverlays`.
- `modules/packages/google-chrome.nix` (modify): wrap the composed
  `CHROME_PROXY` package with `final.wrapShittyShit`.
- `modules/programs/slack.nix` (modify): wrap the icon-fixup package with
  `final.wrapShittyShit`.
- `modules/gui.nix` (modify): import the home module and enable it when Slack
  or Chrome is enabled.

---

## Task 1: Launcher script `shitty-shit-run`

**Files:**
- Create: `files/bin/shitty-shit-run`

**Interfaces:**
- Consumes: nothing (leaf).
- Produces: an executable contract used by Task 2's package and Task 3's
  wrapper — CLI form `shitty-shit-run <proxy-filter-flags…> -- <cmd> [args…]`.
  Everything before `--` is passed verbatim to `xdg-dbus-proxy … --filter`;
  everything after `--` is the command to run inside the slice.

This task only writes the file; it is built and shellcheck-verified in Task 2
(the script is not a standalone package until wrapped by `writeShellApplication`).

- [ ] **Step 1: Write the launcher script body**

Create `files/bin/shitty-shit-run` with exactly this content (no shebang, no
`set` line — `writeShellApplication` prepends `#!bash` and
`set -o errexit -o nounset -o pipefail`):

```bash
# shellcheck shell=bash

# Everything before `--` is this app's xdg-dbus-proxy whitelist, baked in by
# wrapShittyShit. org.freedesktop.systemd1 is denied simply by never being
# granted, so Chromium/Electron cannot StartTransientUnit and therefore cannot
# escape into their own app-*.scope under app.slice.
# Upstream bug: https://issues.chromium.org/issues/437667316
#   "Detect if being started in a systemd unit and do not create own scope."
allow=()
while [ "$#" -gt 0 ] && [ "$1" != "--" ]; do
  allow+=("$1")
  shift
done
[ "${1:-}" = "--" ] && shift
if [ "$#" -eq 0 ]; then
  echo "shitty-shit-run: no command after --" >&2
  exit 64
fi

# Fallback: no usable user session bus -> run directly, no isolation.
if ! systemctl --user show-environment >/dev/null 2>&1; then
  exec "$@"
fi

runtime="${XDG_RUNTIME_DIR:?XDG_RUNTIME_DIR must be set}"
upstream="${DBUS_SESSION_BUS_ADDRESS:-unix:path=${runtime}/bus}"

name="${1##*/}"
dir="${runtime}/shitty-shit"
mkdir -p "$dir"
sock="${dir}/${name}.$$.bus"
rm -f "$sock"

# Per-launch filtering proxy. With an empty whitelist only the bus driver is
# reachable; systemd1 is never granted.
xdg-dbus-proxy "$upstream" "$sock" --filter "${allow[@]}" &
proxy=$!
trap 'kill "$proxy" 2>/dev/null || true; rm -f "$sock"' EXIT

# Wait (bounded, ~2.5s) for the proxy socket to appear.
tries=0
while [ ! -S "$sock" ] && [ "$tries" -lt 50 ]; do
  sleep 0.05
  tries=$((tries + 1))
done

# Launch INTO the slice. systemd-run --user must use the REAL bus to create the
# scope (it talks to systemd1), so we do NOT override its DBUS address. Only the
# app gets the filtered socket, applied with `env` inside the scope.
rc=0
systemd-run --user --scope --collect --same-dir \
  --slice=shitty-shit.slice \
  -- env DBUS_SESSION_BUS_ADDRESS="unix:path=${sock}" "$@" || rc=$?
exit "$rc"
```

- [ ] **Step 2: Commit**

```bash
git add files/bin/shitty-shit-run
git commit -m "feat(shitty-shit): add shitty-shit-run launcher script"
```

---

## Task 2: `shitty-shit-launcher` overlay — package + `wrapShittyShit`

**Files:**
- Create: `modules/shitty-shit-slice.nix`
- Modify: `modules/nix.nix` (add overlay to `defaultOverlays`)

**Interfaces:**
- Consumes: `files/bin/shitty-shit-run` (Task 1) via `self.lib.self.read`.
- Produces:
  - `pkgs.shitty-shit-run` — a `writeShellApplication` (mainProgram
    `shitty-shit-run`).
  - `pkgs.wrapShittyShit` — function
    `pkg: { talk ? [], own ? [], see ? [] }: derivation`. Returns a package
    where every `bin/*` is a wrapper `exec shitty-shit-run <flags> -- <pkg>/bin/<name> "$@"`
    and every `.desktop` `Exec=` first token is repointed to `$out/bin/<basename>`
    (trailing args/field codes preserved). `meta` (incl. `mainProgram`) is
    preserved; `passthru.unwrapped` is the original `pkg`.

- [ ] **Step 1: Create the module with the overlay only (no home module yet)**

Create `modules/shitty-shit-slice.nix`:

```nix
{ self, lib, ... }:
let
  selfLib = self.lib.self;
in
{
  flake.overlays.shitty-shit-launcher = final: _prev: {
    shitty-shit-run = final.writeShellApplication {
      name = "shitty-shit-run";
      runtimeInputs = [
        final.systemd # systemctl, systemd-run
        final.xdg-dbus-proxy
        final.coreutils # env, mkdir, rm, sleep
      ];
      text = selfLib.read "bin/shitty-shit-run";
    };

    wrapShittyShit =
      pkg:
      {
        talk ? [ ],
        own ? [ ],
        see ? [ ],
      }:
      let
        flags = lib.escapeShellArgs (
          (map (n: "--talk=${n}") talk)
          ++ (map (n: "--own=${n}") own)
          ++ (map (n: "--see=${n}") see)
        );
        runner = lib.getExe final.shitty-shit-run;
      in
      final.runCommandLocal "${pkg.name}-shitty"
        {
          meta = (pkg.meta or { });
          passthru = (pkg.passthru or { }) // { unwrapped = pkg; };
        }
        ''
          # Writable clone of the package as a symlink tree.
          mkdir -p "$out"
          cp -as --no-preserve=mode "${pkg}/." "$out/"

          # Replace every bin/* with a wrapper that routes through the launcher.
          if [ -d "$out/bin" ]; then
            for bin in "$out"/bin/*; do
              name="$(basename "$bin")"
              rm -f "$bin"
              {
                echo "#!${final.runtimeShell}"
                echo "exec ${runner} ${flags} -- ${pkg}/bin/$name \"\$@\""
              } > "$bin"
              chmod +x "$bin"
            done
          fi

          # Repoint every .desktop Exec= first token at our wrapper, preserving
          # trailing args and field codes (handles Desktop Actions too).
          if [ -d "$out/share/applications" ]; then
            for d in "$out"/share/applications/*.desktop; do
              [ -e "$d" ] || continue
              ${final.gawk}/bin/awk -v out="$out" '
                /^Exec=/ {
                  cmd = substr($0, 6)
                  n = split(cmd, a, " ")
                  exe = a[1]; sub(/.*\//, "", exe)
                  rest = ""
                  for (i = 2; i <= n; i++) rest = rest " " a[i]
                  print "Exec=" out "/bin/" exe rest
                  next
                }
                { print }
              ' "$d" > "$d.tmp"
              mv "$d.tmp" "$d"
            done
          fi
        '';
  };
}
```

- [ ] **Step 2: Register the overlay in `defaultOverlays`**

In `modules/nix.nix`, find the `defaultOverlays` list (around line 21). Add
`self.overlays.shitty-shit-launcher` immediately before `self.overlays.slack`:

```nix
    self.overlays.shitty-shit-launcher
    self.overlays.slack
```

(Order is not load-bearing — the chrome/slack overlays reference
`final.wrapShittyShit`, the composed fixpoint — but keeping it adjacent to the
consumers is clearest.)

- [ ] **Step 3: Build `shitty-shit-run` (runs shellcheck)**

Run: `nix build --no-link --print-out-paths '.#configured-pkgs.x86_64-linux.nixpkgs.shitty-shit-run'`
Expected: builds successfully (shellcheck passes), prints a store path.

- [ ] **Step 4: Inspect the built launcher**

Run:
```bash
p=$(nix build --no-link --print-out-paths '.#configured-pkgs.x86_64-linux.nixpkgs.shitty-shit-run')
sed -n '1,60p' "$p"/bin/shitty-shit-run
```
Expected: shebang + `set -o errexit -o nounset -o pipefail`, the whitelist
parse loop, the `xdg-dbus-proxy … --filter` line, and the
`systemd-run --user --scope … -- env DBUS_SESSION_BUS_ADDRESS=… "$@"` line.

- [ ] **Step 5: Smoke-test `wrapShittyShit` on a tiny package**

Run:
```bash
nix build --no-link --print-out-paths \
  --expr 'let p = (builtins.getFlake (toString ./.)).configured-pkgs.x86_64-linux.nixpkgs; in p.wrapShittyShit p.hello {}' --impure
```
Expected: builds. Then inspect the wrapper:
```bash
q=$(nix build --no-link --print-out-paths --impure \
  --expr 'let p = (builtins.getFlake (toString ./.)).configured-pkgs.x86_64-linux.nixpkgs; in p.wrapShittyShit p.hello {}')
cat "$q"/bin/hello
```
Expected: a 2-line script whose last line is
`exec /nix/store/…-shitty-shit-run/bin/shitty-shit-run -- /nix/store/…-hello-*/bin/hello "$@"`
(empty flags because whitelist is empty; note the `--` then the real hello path).

- [ ] **Step 6: Commit**

```bash
git add modules/shitty-shit-slice.nix modules/nix.nix
git commit -m "feat(shitty-shit): add shitty-shit-run package and wrapShittyShit overlay"
```

---

## Task 3: Slice unit + `programs.shitty-shit-slice.enable` home module

**Files:**
- Modify: `modules/shitty-shit-slice.nix` (add `flake.homeModules.shitty-shit-slice`)

**Interfaces:**
- Consumes: nothing new.
- Produces: `self.homeModules.shitty-shit-slice`, defining
  `options.programs.shitty-shit-slice.enable` (bool) and, when enabled, writing
  `~/.config/systemd/user/shitty-shit.slice`.

- [ ] **Step 1: Add the home module**

In `modules/shitty-shit-slice.nix`, add a second attribute alongside
`flake.overlays.shitty-shit-launcher` (inside the same top-level attrset):

```nix
  flake.homeModules.shitty-shit-slice =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.home.shitty-shit-slice";

      options.programs.shitty-shit-slice.enable =
        lib.mkEnableOption "shitty-shit.slice (memory-capped Slack + Chrome)";

      config = lib.mkIf config.programs.shitty-shit-slice.enable {
        xdg.configFile."systemd/user/shitty-shit.slice".text = ''
          [Unit]
          Description=Memory-capped slice for Slack + Chrome

          [Slice]
          MemoryHigh=8G
          MemoryMax=10G
          MemorySwapMax=0
        '';
      };
    };
```

- [ ] **Step 2: Evaluate the flake to confirm the module parses**

Run: `nix eval --raw '.#homeModules.shitty-shit-slice' --apply 'm: "ok"' 2>&1 | tail -3`
Expected: prints `ok` (the module attribute exists and the flake evaluates).

If `--apply` on a module function errors, fall back to:
Run: `nix eval '.#homeModules' --apply 'builtins.attrNames' 2>&1 | tr ',' '\n' | grep shitty`
Expected: `shitty-shit-slice` appears in the list.

- [ ] **Step 3: Commit**

```bash
git add modules/shitty-shit-slice.nix
git commit -m "feat(shitty-shit): add slice unit + programs.shitty-shit-slice.enable"
```

---

## Task 4: Wrap Slack

**Files:**
- Modify: `modules/programs/slack.nix`

**Interfaces:**
- Consumes: `final.wrapShittyShit` (Task 2).
- Produces: `pkgs.slack` whose `bin/slack` routes through `shitty-shit-run` and
  whose `.desktop` `Exec=` points at the wrapper.

- [ ] **Step 1: Wrap the icon-fixup package**

Replace the body of `modules/programs/slack.nix` so the overlay wraps its
result. The current file builds `prev.slack.overrideAttrs (…)` (icon fixup);
bind that to `slackWithIcons` and wrap it:

```nix
{ self, ... }:
{
  flake.overlays.slack = final: prev: {
    slack =
      let
        slackWithIcons = prev.slack.overrideAttrs (old: {
          installPhase = old.installPhase + ''
            local icons="$out/lib/slack/resources/app.asar.unpacked/dist/resources"
            cp "$icons/slack-taskbar-rest.ico" "$icons/slack-taskbar-unread.ico"
            cp "$icons/slack-taskbar-rest.ico" "$icons/slack-taskbar-highlight.ico"
          '';
        });
      in
      final.wrapShittyShit slackWithIcons {
        talk = [ ];
        own = [ ];
        see = [ ];
      };
  };
}
```

- [ ] **Step 2: Build the wrapped Slack**

Run: `nix build --no-link --print-out-paths '.#configured-pkgs.x86_64-linux.nixpkgs.slack'`
Expected: builds, prints a store path.

- [ ] **Step 3: Verify bin + desktop rewrite**

Run:
```bash
s=$(nix build --no-link --print-out-paths '.#configured-pkgs.x86_64-linux.nixpkgs.slack')
echo "--- bin/slack ---"; cat "$s"/bin/slack
echo "--- desktop Exec ---"; grep -h '^Exec=' "$s"/share/applications/*.desktop
```
Expected:
- `bin/slack` last line: `exec …/shitty-shit-run/bin/shitty-shit-run -- /nix/store/…-slack-*/bin/slack "$@"`
- desktop Exec: `Exec=<store>/…-slack-*-shitty/bin/slack -s %U` (first token
  repointed into the wrapped output's `bin/`, `-s %U` preserved).

- [ ] **Step 4: Commit**

```bash
git add modules/programs/slack.nix
git commit -m "feat(shitty-shit): route Slack through wrapShittyShit"
```

---

## Task 5: Wrap Chrome

**Files:**
- Modify: `modules/packages/google-chrome.nix`

**Interfaces:**
- Consumes: `final.wrapShittyShit` (Task 2).
- Produces: `pkgs.google-chrome` whose `bin/google-chrome-stable` (the
  `CHROME_PROXY` wrapper) is further wrapped through `shitty-shit-run`, and
  whose `.desktop` entries (including Desktop Actions) point at the wrapper.

- [ ] **Step 1: Wrap the CHROME_PROXY package and drop the bespoke desktop sed**

`wrapShittyShit` now performs the `.desktop` rewrite, so the old `desktopFiles`
`sed` step is redundant. Replace `modules/packages/google-chrome.nix` with:

```nix
{ lib, ... }:
{
  flake.overlays.my-google-chrome = final: prev: {
    google-chrome =
      let
        chromeWrapper = final.writeShellScriptBin "google-chrome-stable" ''
          if [[ -n "''${CHROME_PROXY:-}" ]]; then
            exec ${lib.getExe prev.google-chrome} --proxy-server="$CHROME_PROXY" "$@"
          else
            exec ${lib.getExe prev.google-chrome} "$@"
          fi
        '';

        chromeBase = final.symlinkJoin {
          name = prev.google-chrome.name;
          paths = [
            chromeWrapper
            prev.google-chrome
          ];
          meta = prev.google-chrome.meta // { mainProgram = "google-chrome-stable"; };
        };
      in
      final.wrapShittyShit chromeBase {
        talk = [ ];
        own = [ ];
        see = [ ];
      };
  };
}
```

Notes:
- `chromeBase`'s `bin/google-chrome-stable` is the `CHROME_PROXY` wrapper
  (wins the symlinkJoin collision as the first path). `wrapShittyShit` then
  wraps it, so the launch chain is
  `shitty-shit-run → CHROME_PROXY wrapper → real chrome` — proxy behavior
  preserved, single slice layer.
- `chromeBase` no longer rewrites desktops; `wrapShittyShit` repoints them.

- [ ] **Step 2: Build the wrapped Chrome**

Run: `nix build --no-link --print-out-paths '.#configured-pkgs.x86_64-linux.nixpkgs.google-chrome'`
Expected: builds, prints a store path.

- [ ] **Step 3: Verify bin + desktop rewrite (incl. Desktop Actions)**

Run:
```bash
c=$(nix build --no-link --print-out-paths '.#configured-pkgs.x86_64-linux.nixpkgs.google-chrome')
echo "--- bin/google-chrome-stable ---"; cat "$c"/bin/google-chrome-stable
echo "--- desktop Exec lines ---"; grep -h '^Exec=' "$c"/share/applications/*.desktop
```
Expected:
- `bin/google-chrome-stable` last line references the `chromeBase` store path
  (the `CHROME_PROXY` wrapper), NOT `$out`:
  `exec …/shitty-shit-run/bin/shitty-shit-run -- /nix/store/…-google-chrome-<ver>/bin/google-chrome-stable "$@"`.
- Every desktop `Exec=` first token points at
  `<store>/…-google-chrome-<ver>-shitty/bin/google-chrome-stable`, with the
  original trailing args (`%U`, `--incognito`, empty) preserved.

- [ ] **Step 4: Commit**

```bash
git add modules/packages/google-chrome.nix
git commit -m "feat(shitty-shit): route Chrome through wrapShittyShit"
```

---

## Task 6: Wire into `gui.nix` + runtime verification

**Files:**
- Modify: `modules/gui.nix`

**Interfaces:**
- Consumes: `self.homeModules.shitty-shit-slice` (Task 3).
- Produces: the slice unit is installed on any home that has Slack or Chrome
  enabled.

- [ ] **Step 1: Import the home module**

In `modules/gui.nix`, find the `imports` list of `flake.homeModules.gui`
(around line 112):

```nix
      imports = [
        self.homeModules.xdg-autostart
        self.homeModules.desktop-essentials
      ];
```

Add the module:

```nix
      imports = [
        self.homeModules.xdg-autostart
        self.homeModules.desktop-essentials
        self.homeModules.shitty-shit-slice
      ];
```

- [ ] **Step 2: Enable the slice when Slack or Chrome is enabled**

In the first `lib.mkMerge` block of `gui.nix` `config` (the block that already
sets `programs.google-chrome.enable = lib.mkDefault true;` and
`programs.slack.enable = lib.mkDefault true;`, around line 128), add:

```nix
            programs.shitty-shit-slice.enable =
              lib.mkDefault (config.programs.slack.enable || config.programs.google-chrome.enable);
```

- [ ] **Step 3: Evaluate the flake (catch wiring/eval errors)**

Run: `nix flake check --no-build 2>&1 | tail -20`
Expected: no evaluation errors referencing `shitty-shit`, `gui`, or
`programs.shitty-shit-slice`. (Unrelated pre-existing warnings are fine.)

If `nix flake check` is too broad/slow in this repo, instead evaluate a home
configuration that includes `gui` and assert the slice file is present. Find
one first:
```bash
grep -rl "self.homeModules.gui" modules/ | head
```
Then build that machine's home activation and check the unit exists (operator
may need to run this if it requires secrets):
```bash
# Example shape — replace <hostAttr> with the actual nixosConfiguration:
nix eval '.#nixosConfigurations.<hostAttr>.config.home-manager.users' --apply 'builtins.attrNames' 2>&1 | tail -3
```

- [ ] **Step 4: Commit**

```bash
git add modules/gui.nix
git commit -m "feat(shitty-shit): enable slice for homes with Slack/Chrome"
```

- [ ] **Step 5: OPERATOR runtime verification (not run by the implementer)**

After the operator rebuilds/deploys a GUI machine, verify on that machine:

1. `systemctl --user daemon-reload` (pick up the new `shitty-shit.slice` unit),
   then launch Slack via the wrapper.
2. `systemd-cgls --user` → the Slack process tree sits under
   `shitty-shit.slice`, and **no** new `app-*.scope` was created under
   `app.slice`.
3. Repeat for Chrome (`google-chrome-stable`).
4. `systemctl --user show shitty-shit.slice -p MemoryHigh -p MemoryMax -p MemorySwapMax`
   → `MemoryHigh=8589934592` (8G), `MemoryMax=10737418240` (10G),
   `MemorySwapMax=0`.
5. With empty whitelists expect dead notifications/tray/portals — that confirms
   the filter is live and `systemd1` is denied. Grow whitelists per app by
   adding names to the `talk`/`own`/`see` lists in `modules/programs/slack.nix`
   and `modules/packages/google-chrome.nix`.
6. Close each app → its `xdg-dbus-proxy` process is gone and its
   `$XDG_RUNTIME_DIR/shitty-shit/<name>.<pid>.bus` socket removed.

**Contingency (documented in the spec):** if Chrome still escapes with an empty
whitelist, it is reaching `systemd1` via the private
`$XDG_RUNTIME_DIR/systemd/private` socket rather than the session bus; the
fallback is to also hide that socket from the app (namespace / `bwrap`). This is
considered unlikely given the validated Slack result.

---

## Self-Review

**Spec coverage:**
- Slice unit `8G/10G/0` (spec §1) → Task 3.
- App-agnostic `shitty-shit-run`, per-launch proxy, `--collect --same-dir`,
  fallback, systemd1 denied (spec §2) → Tasks 1–2. **Correction vs. spec
  sketch:** the filtered bus is applied to the app via `env` inside the scope,
  not to `systemd-run` itself (which needs the real `systemd1`). Captured in
  Global Constraints + Task 1.
- `wrapShittyShit pkg { talk; own; see; }` wrapping every `bin/*` + `.desktop`
  (spec §3) → Task 2.
- Chrome integration composing over CHROME_PROXY (spec §4) → Task 5.
- Slack integration over icon fixup (spec §5) → Task 4.
- Packaging: overlay in `defaultOverlays`, `programs.shitty-shit-slice.enable`,
  `gui.nix` gating (spec §6) → Tasks 2, 3, 6.
- Verification plan (spec) → Task 6 Step 5 (operator).
- Risks/contingency (spec) → Task 6 contingency note.

**Placeholder scan:** none — every step has concrete file content or exact
commands with expected output.

**Type consistency:** `shitty-shit-run` CLI contract (`<flags> -- <cmd>`) is
identical across Task 1 (script), Task 2 (`wrapShittyShit` emits
`shitty-shit-run <flags> -- <pkg>/bin/<name> "$@"`), and Tasks 4/5 (consume the
wrapper). `wrapShittyShit`'s signature `pkg: { talk?; own?; see?; }` matches its
call sites in Tasks 4 and 5. `flake.homeModules.shitty-shit-slice` (Task 3) is
consumed as `self.homeModules.shitty-shit-slice` in Task 6.
```
