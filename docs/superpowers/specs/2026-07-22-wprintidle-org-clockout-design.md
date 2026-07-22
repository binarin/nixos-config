# Seat-level (Wayland) idle for `org-clock-auto-clockout`

Date: 2026-07-22
Status: Approved (design)

## Problem

`org-clock-auto-clockout-timer` (set to 3600s, insinuated in `b-org.el`)
clocks out the running clock after a configurable idle period. It calls
`org-user-idle-seconds` to determine idleness. That function checks, in order:
mac → X11 (`org-clock-x11idle-program-name`) → w32 → **logind `IdleHint`** →
fallback to `org-emacs-idle-seconds` (Emacs-only idle).

Under this setup the compositor is **niri** and idle is managed by **swayidle**.
swayidle does *not* drive logind's `IdleHint` (confirmed: `IdleHint=no`
persistently), and X11 is unavailable on the pgtk build, so `org-user-idle-seconds`
falls through to **Emacs-only idle**. Consequence: reading a long page in a
browser, watching a video, etc. makes Emacs "idle" even though the user is
active, so the clock can auto-clockout while the user is genuinely working.

We want idleness tracked at the **seat level** (no keyboard/mouse activity for
the whole session), which is what `org-clock-auto-clockout-timer` was meant to
capture.

### Scope decisions (from brainstorming)

- **What counts as idle:** pure input-idle at the seat, with **no** regard for
  idle inhibitors. Rationale: inhibition is used to keep the screen from
  blanking (video, games); the user still wants the clock to clock out after
  the timeout because they are genuinely not working.
- **swayidle is not the source:** its timers are event-driven (they don't yield
  a number) and are intentionally inhibited in exactly the situations where we
  still want to clock out.
- **Failure mode:** when the idle provider is unavailable, **silently** fall
  back to Org's built-in `org-user-idle-seconds` (option A) — no warnings.

## Approach

Wire a Wayland-native idle provider into Org's existing idle-detection chain
via `:around` advice on `org-user-idle-seconds`. This is a **pure addition**:
on builds/displays where the provider is unavailable, the advice returns nil
and defers to Org's original function, so non-Wayland emacsen (nox, X11)
behave exactly as before. No patch to Org or Emacs source; no reimplementation
of Org's auto-clockout timer logic.

The provider is **`wprintidle-c`** (`../wprintidle-c/`, a non-flake input): a
small C daemon+client implementing the `ext-idle-notify-v1` Wayland protocol.
The daemon maintains idle state on a Unix socket
(`$XDG_RUNTIME_DIR/wprintidle-c.sock`); the client queries it. The daemon's
protocol is trivially line-based:

- send `QUERY_SECONDS\n`
- receive one line like `42\n` (integer seconds)

Rather than fork+exec the client on each poll, Emacs **connects to the daemon
socket directly** (`make-network-process :family 'local`, the same idiom
`server.el` uses), which is cheaper and lets us enforce a timeout.

## Components

### 1. Package — `modules/packages/wprintidle-c.nix`

Mirror of `modules/packages/lan-mouse.nix` (input declaration + overlay
together in `modules/packages/`, not `self.packages`):

- `flake-file.inputs.wprintidle-c` — local non-flake input pointing at
  `../wprintidle-c`, `flake = false`.
- `flake.overlays.wprintidle-c = final: prev: { wprintidle-c = …; }` building
  the derivation via `callPackage`. Build inputs: `wayland` (runtime
  `libwayland-client`), `wayland-scanner` + `pkg-config` (nativeBuildInputs).
  The vendored `Makefile` already drives `wayland-scanner` over the bundled
  `protocol/ext-idle-notify-v1.xml`, so no extra protocol xml is needed.
- Install **only the two binaries** (`wprintidle-c-daemon`, `wprintidle-c`);
  do not install the shipped `wprintidle-c.service` (the home-manager module
  provides its own unit). Use `make PREFIX=$out install` if it cooperates, else
  install the two binaries manually into `$out/bin` with `install -D`.
- `meta.mainProgram = "wprintidle-c"`.
- Register `self.overlays.wprintidle-c` in `defaultOverlays` (`modules/nix.nix`),
  alongside `self.overlays.waybar-org-clock`.

### 2. home-manager module — `modules/binarin/wprintidle.nix`

Mirror of `modules/binarin/swayidle.nix`; defines `self.homeModules.wprintidle`:

- `options.services.wprintidle.enable = mkEnableOption "wprintidle-c idle tracking";`
- when enabled:
  - `home.packages = [ pkgs.wprintidle-c ]` — puts the client binary on PATH
    for emacs/emacsclient.
  - `systemd.user.services.wprintidle-c`:
    - `Unit`: `PartOf = [ "graphical-session.target" ]`;
      `After = [ "graphical-session.target" ]`;
      `Wants = [ "graphical-session.target" ]`.
    - `Service`: `ExecStart = "${pkgs.wprintidle-c}/bin/wprintidle-c-daemon 1000"`
      (the `1000` is the daemon's "considered idle" granularity in ms — *not*
      the clock-out timeout, which stays at Org's 3600s; default to upstream's
      1000ms). `Restart = "on-failure"`; `RestartSec = 5`.
    - `Install`: `WantedBy = [ "graphical-session.target" ]`.

Gated to graphical/Wayland hosts: enabled only where imported and turned on.
Non-graphical hosts never import it, so the daemon never starts there and the
elisp falls back to built-in idle.

### 3. elisp — `files/emacs/user-lisp/b-org.el`

- `b/wprintidle-socket` — lazily computes the socket path, mirroring
  `wprintidle-c/src/common.c`: `$XDG_RUNTIME_DIR/wprintidle-c.sock`, with a
  `/tmp/wprintidle-c-<uid>.sock` fallback when `XDG_RUNTIME_DIR` is unset.
- `b/wayland-idle-seconds` — opens the socket directly and returns a float,
  or nil on any failure (see Error handling below).
- `b/org-user-idle-seconds-around` — the `:around` advice: returns the
  Wayland value when non-nil, else calls the original `org-user-idle-seconds`.
- `(advice-add #'org-user-idle-seconds :around #'b/org-user-idle-seconds-around)`.
- Follow the project's autoloads/byte-compilation discipline (`.ai/EMACS.md`).

### 4. Wiring

Import `self.homeModules.wprintidle` and set `services.wprintidle.enable = true`
in the graphical host config — the same place niri/swayidle are enabled
(`modules/binarin/workstation.nix` or a machine module; exact site to be
confirmed during implementation).

## Data flow

```
wprintidle-c-daemon (systemd user service)
   │  ext-idle-notify-v1 on wl_seat
   │  Unix socket: $XDG_RUNTIME_DIR/wprintidle-c.sock
   ▼
org-clock--auto-clockout-maybe  (existing, untouched)
   │  (org-user-idle-seconds)
   ▼
org-user-idle-seconds + :around advice  (new, b-org.el)
   │  b/wayland-idle-seconds:
   │    1. (featurep 'make-network-process '(:family local)) gate
   │    2. resolve socket path (XDG_RUNTIME_DIR | /tmp fallback)
   │    3. make-network-process :family 'local (with sentinel)
   │    4. process-send-string "QUERY_SECONDS\n"
   │    5. accept-process-output with ~0.1s timeout
   │    6. parse single integer line → float, or nil
   │    7. unwind-protect: delete-process always
   │  nil → fall through to original org-user-idle-seconds
   ▼
compared against org-clock-auto-clockout-timer (3600s)
   │  >= threshold → org-clock-out
   │  <  threshold → re-arm (Org's existing retry logic, untouched)
```

No persistent connection: open/send/receive/close each poll. Polling is
infrequent (only while a clock is armed), so this is cheap and avoids
reconnection/reconnect-on-resume logic (YAGNI).

## Error handling

Every failure path in `b/wayland-idle-seconds` returns `nil` → silent fallback
to built-in `org-user-idle-seconds`:

- `make-network-process` not featureful (`(:family local)` unsupported) → nil.
- Socket path unresolvable (`XDG_RUNTIME_DIR` unset on a tty host) → nil.
- `make-network-process` signals (connection refused — daemon down) →
  `condition-case` catches it → nil.
- `accept-process-output` times out (daemon hung) → nil.
- Response unparseable (non-numeric / empty) → nil.
- All wrapped in `unwind-protect` so `delete-process` always runs — no leaked
  process or socket fd on any failure.

Build failures (nix) surface normally; runtime daemon crashes recover via
`Restart=on-failure`.

## Testing

- **Package build:** `nix build .#wprintidle-c` (overlay → nixpkgs attribute);
  confirm both binaries exist and `--help` works; `nix run` against a live
  Wayland session to confirm seconds output.
- **HM module:** apply to a graphical host, `systemctl --user status
  wprintidle-c`, confirm `$XDG_RUNTIME_DIR/wprintidle-c.sock` exists and
  `wprintidle-c` returns a number.
- **elisp (pure pieces, unit-testable):**
  - Socket-path computation: both `XDG_RUNTIME_DIR` and `/tmp` fallback
    branches via `let`-binding.
  - Response parser: `"42\n"` → 42.0; `""` → nil; `"garbage"` → nil.
  - Fallback: with the socket path pointed at a nonexistent file,
    `b/wayland-idle-seconds` returns nil within the timeout without hanging,
    and the composed around-advice returns whatever the original returns.
- **End-to-end (manual):** clock in, idle past the threshold, confirm
  auto-clockout fires; on a non-Wayland host, confirm built-in behavior
  unchanged.
