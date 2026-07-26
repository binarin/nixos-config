# niri-aware `other-window` frame focus sync

## Problem

`C-x o` (`other-window`) only updates Emacs's internal notion of the
selected frame. When it lands in a window belonging to a *different*
Emacs frame, the Wayland (niri) keyboard focus stays on the old frame —
so the next keystroke is delivered to the wrong window and focus
visibly snaps back. The result: `C-x o` across frames feels broken
inside niri.

`emacs-niri-awareness` already provides the building blocks:
- `niri-frame-niri-id` (frame → niri window id)
- `niri-rpc-focus-window` (send `FocusWindow` action)

What's missing is a predicate to cheaply test whether the niri IPC
connection is live (the existing `niri-rpc--ensure-connected` *errors*),
and a command that ties frame-change detection to a focus sync.

## Design

Two changes, in two repositories.

### 1. `niri-rpc-connected-p` — new predicate

Add to `emacs-niri-awareness/niri-rpc.el`:

```elisp
;;;###autoload
(defun niri-rpc-connected-p ()
  "Return non-nil when the niri IPC connection is live."
  (and niri-rpc--async-process
       (eq (process-status niri-rpc--async-process) 'open)))
```

A pure predicate (no error), factoring out the exact liveness test
currently inlined in `niri-rpc--ensure-connected`. `ensure-connected`
is left as-is (it stays the error-raising variant for code that wants
to fail fast); both may share the same underlying test via the new
function if convenient, but behavior of `ensure-connected` is
unchanged.

### 2. `niri-other-window` wrapper + repeat map — in `nixos-config`

Add to `files/emacs/user-lisp/l-windows.el`. The wrapper:

- Captures `(selected-frame)` before delegating.
- Calls `other-window` with the forwarded arguments (so `C-u N C-x o`
  and the `all-frames` interactive path still work).
- After `other-window` returns, **only** if **both**:
  - `(niri-rpc-connected-p)` is non-nil, and
  - the new `(selected-frame)` differs from the captured frame,
  
  look up `(niri-frame-niri-id (selected-frame))`; if non-nil, call
  `(niri-rpc-focus-window id)` to sync Wayland focus.
- No `FocusWindow` is sent when the frame did not change (niri focus is
  already correct), and nothing happens when the niri id is missing
  (e.g. a frame not yet mapped to a niri window) — silently degrade.

The wrapper is bound via `(keymap-global-set [remap other-window] #'…)`
or the project's preferred binding form, so `C-x o` and any other
binding that points at `other-window` are routed through it.

A dedicated repeat map is declared so `repeat-mode` keeps working and
has room to grow:

```elisp
(defvar-keymap niri-other-window-repeat-map
  :doc "Repeat map for `niri-other-window'."
  :repeat t
  "o" #'niri-other-window
  "O" (lambda () (interactive) (niri-other-window -1)))

(put 'niri-other-window 'repeat-map 'niri-other-window-repeat-map)
```

`:repeat t` lets the map's `o`/`O` entries themselves repeat (mirroring
the built-in `other-window-repeat-map`). Every repeat press re-runs the
frame-change check, so chaining `C-x o o o …` across frames syncs niri
focus on each hop.

### Failure mode

The niri focus sync is guarded *only* by `niri-rpc-connected-p`. No
`condition-case`. If the predicate is true but the subsequent
`FocusWindow` call somehow errors, that error propagates. Rationale:
`niri-rpc-connected-p` is the single, well-defined gate; beyond it we
trust the IPC path. This matches the user's preference for a minimal
guard rather than a defensive swallow-all.

## Files touched

| Repo | File | Change |
|---|---|---|
| `emacs-niri-awareness` | `niri-rpc.el` | add `niri-rpc-connected-p` (with `;;;###autoload`) |
| `nixos-config` | `files/emacs/user-lisp/l-windows.el` | add `niri-other-window` command, repeat map, `[remap other-window]` binding |

## Testing

- Unit-test the predicate and the frame-change guard in
  `emacs-niri-awareness` style (extend `niri-rpc-test.el` / add a small
  test for the wrapper's no-op-when-disconnected path).
- Manual: under niri with multiple Emacs frames, `C-x o` to a window in
  another frame must move Wayland focus (visible cursor / immediate
  typing lands in the new frame). `C-x o` within a single frame must
  still work and issue no `FocusWindow`. Chaining `C-x o o o` must
  repeat correctly.

## Temporary dev override

While iterating, set (in `flake.nix` only — do not commit the override):

```nix
emacs-niri-awareness.url = "path:/home/binarin/personal-workspace/emacs-niri-awareness";
```

so `nixos-config` can consume the new predicate before it's published.
Revert before final commit.
