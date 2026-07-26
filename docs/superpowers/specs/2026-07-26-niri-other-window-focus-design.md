# niri-aware `other-window` frame focus sync

## Problem

The user binds `C-x o` to `(lambda () (interactive) (other-window
1 'visible))` — passing `'visible` as `all-frames` so navigation skips
minimized/iconified frames. `other-window` only updates Emacs's
internal notion of the selected frame, though: when it lands in a
window belonging to a *different* Emacs frame, the Wayland (niri)
keyboard focus stays on the old frame — so the next keystroke is
delivered to the wrong window and focus visibly snaps back. The
result: `C-x o` across frames feels broken inside niri.

Worse, that binding is currently an anonymous lambda, so `repeat-mode`
does nothing for it (the built-in `other-window-repeat-map` exists in
`window.el`, but it binds the bare `other-window`/`other-window-backward`,
which don't pass `'visible`).

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

### 2. `b/other-window` wrapper + repeat map — in `nixos-config`

Add to `files/emacs/user-lisp/l-windows.el`. The wrapper, `b/other-window` (with `;;;###autoload`), takes a `count`
(defaults to 1) and:

- Captures `(selected-frame)` before delegating.
- Calls `(other-window count 'visible)` — **always** passing `'visible`
  as `all-frames`, which is the whole point of the user's binding.
- After `other-window` returns, **only** if **both**:
  - `(niri-rpc-connected-p)` is non-nil, and
  - the new `(selected-frame)` differs from the captured frame,
  
  look up `(niri-frame-niri-id (selected-frame))`; if non-nil, call
  `(niri-rpc-focus-window id)` to sync Wayland focus.
- No `FocusWindow` is sent when the frame did not change (niri focus is
  already correct), and nothing happens when the niri id is missing
  (e.g. a frame not yet mapped to a niri window) — silently degrade.

`C-x o` is bound via `bind-key [remap other-window] #'b/other-window`
in `init.el`. Remap affects only key lookup (the default `C-x o` and
any other key bound to `other-window`), not direct Lisp calls — so
code that calls `(other-window ...)` keeps its own `all-frames`
semantics, while the user's `C-x o` goes through `b/other-window`.
This replaces the user's current anonymous lambda.

A dedicated repeat map is declared so `repeat-mode` keeps working and
has room to grow. Crucially, the repeat entries route through
`b/other-window` (not the bare built-in), so `'visible` and the
focus sync apply on every repeat:

```elisp
(defvar-keymap b/other-window-repeat-map
  :doc "Repeat map for `b/other-window'.  Used in `repeat-mode'."
  :repeat t
  "o" #'b/other-window
  "O" (lambda () (interactive) (b/other-window -1)))

(put 'b/other-window 'repeat-map 'b/other-window-repeat-map)
```

`:repeat t` mirrors the built-in `other-window-repeat-map`. Every
repeat press re-runs the frame-change check, so chaining
`C-x o o o …` across frames syncs niri focus on each hop.

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
| `nixos-config` | `files/emacs/user-lisp/l-windows.el` | add `b/other-window` command (`;;;###autoload`), `b/other-window-repeat-map` |
| `nixos-config` | `files/emacs/init.el` | `bind-key [remap other-window] #'b/other-window` |

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
