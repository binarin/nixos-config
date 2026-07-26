# niri-aware `other-window` frame focus sync — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `C-x o` (`other-window`) sync Wayland (niri) keyboard focus when it crosses into a different Emacs frame, so typing lands in the frame you just moved to.

**Architecture:** Add a pure `niri-rpc-connected-p` predicate to `emacs-niri-awareness/niri-rpc.el`. In `nixos-config`, add a `b/other-window` command (always passes `'visible`, syncs niri focus on frame change) plus a dedicated repeat map to `l-windows.el`, and bind `C-x o` to it via `[remap other-window]` in `init.el`.

**Tech Stack:** Emacs Lisp (lexical binding), ERT, `bind-key`, `defvar-keymap`, `repeat-mode`. Two repos: `emacs-niri-awareness` (library) and `nixos-config` (consumer).

## Global Constraints

- Lexical binding (`;; -*- lexical-binding: t; -*-`) in every new/edited Elisp file — already present in both target files.
- Byte-compilation runs with `byte-compile-error-on-warn t` in `nixos-config` (see `.ai/EMACS.md`). Any function from another package must be declared with `declare-function` or `require`d.
- `emacs-niri-awareness` unit tests run via `./run-unit-tests.sh` (no niri required). New unit tests must be added to the hardcoded test list inside that script.
- `nixos-config` emacs tests run via `bash scripts/test-emacs.sh` (nix `.#emacs-test` shell, bubblewrap sandbox, `byte-compile-error-on-warn t`). It uses `shopt -s globstar`, so `files/emacs/tests/**/*.el` are discovered automatically.
- Naming prefix `b/` for feature commands (convention in `files/emacs/user-lisp/l-windows.el`).
- `;;;###autoload` on any command meant to be reachable before its module is loaded (autoloads are auto-generated, never committed — see `.ai/EMACS.md`).
- The user's current live binding `(bind-key "C-x o" (lambda () (interactive) (other-window 1 'visible)))` is replaced by this work.
- For testing against the new predicate before publishing, `emacs-niri-awareness.url` in `flake.nix` may be temporarily overridden to a `path:` URL — but that override must NOT be committed.

## File Structure

| Repo | File | Responsibility |
|---|---|---|
| `emacs-niri-awareness` | `niri-rpc.el` | IPC client; add `niri-rpc-connected-p` predicate |
| `emacs-niri-awareness` | `niri-rpc-test.el` | unit tests for the predicate |
| `emacs-niri-awareness` | `run-unit-tests.sh` | hardcoded unit-test list (add new test names here) |
| `nixos-config` | `files/emacs/user-lisp/l-windows.el` | `b/other-window` + `b/other-window-backward` commands, `b/other-window-repeat-map` |
| `nixos-config` | `files/emacs/tests/test-l-windows.el` | unit tests for the wrapper |
| `nixos-config` | `files/emacs/init.el` | `(bind-key [remap other-window] #'b/other-window)` |

## Interfaces

- **`niri-rpc-connected-p`** (Task 1 produces): `(defun niri-rpc-connected-p ())` returns non-nil when `niri-rpc--async-process` is non-nil and `(process-status niri-rpc--async-process)` is `open`; returns `nil` otherwise. No args. Never signals. Lives in `niri-rpc.el`, autoloaded.
- **`b/other-window`** (Task 4 produces): `(defun b/other-window (&optional count))` autoloaded command. `COUNT` defaults to 1 (interactive `"p"`). Calls `(other-window (or count 1) 'visible)`. On frame change while `(niri-rpc-connected-p)` returns non-nil, calls `(niri-rpc-focus-window id)` where `id` is `(niri-frame-niri-id (selected-frame))`, but only when that id is non-nil. Consumes `niri-rpc-connected-p`, `niri-frame-niri-id`, `niri-rpc-focus-window`.
- **`b/other-window-backward`** (Task 4 produces): `(defun b/other-window-backward (&optional count))` autoloaded; `(b/other-window (- (or count 1)))`.
- **`b/other-window-repeat-map`** (Task 4 produces): keymap with `:repeat t`, `"o"`→`b/other-window`, `"O"`→`b/other-window-backward`. Both commands carry `(put <cmd> 'repeat-map 'b/other-window-repeat-map)`.

---

### Task 1: Add `niri-rpc-connected-p` predicate

**Files:**
- Modify: `emacs-niri-awareness/niri-rpc.el` (insert immediately before the existing `niri-rpc--ensure-connected` defun, currently around line 752)
- Modify: `emacs-niri-awareness/niri-rpc-test.el` (append one test)
- Modify: `emacs-niri-awareness/run-unit-tests.sh` (add the test name to `rpc_unit`)

**Interfaces:**
- Produces: `niri-rpc-connected-p` (see Interfaces section above).

- [ ] **Step 1: Write the failing unit test**

Append to `emacs-niri-awareness/niri-rpc-test.el`:

```elisp
(ert-deftest niri-rpc-connected-p-disconnected ()
  "niri-rpc-connected-p is nil when there is no live process."
  (let ((niri-rpc--async-process nil))
    (should (null (niri-rpc-connected-p)))))
```

Add `niri-rpc-connected-p-disconnected` to the `rpc_unit` array in `emacs-niri-awareness/run-unit-tests.sh`.

- [ ] **Step 2: Run the test to verify it fails**

Run:
```bash
cd /home/binarin/personal-workspace/emacs-niri-awareness && ./run-unit-tests.sh
```
Expected: FAIL on `niri-rpc-connected-p-disconnected` — "void-function niri-rpc-connected-p".

- [ ] **Step 3: Implement the predicate**

In `emacs-niri-awareness/niri-rpc.el`, insert immediately **before** the existing `niri-rpc--ensure-connected` defun:

```elisp
;;;###autoload
(defun niri-rpc-connected-p ()
  "Return non-nil when the niri IPC connection is live.
A connection is live when `niri-rpc--async-process' is non-nil
and its process status is `open'.  This is a pure predicate and
never signals; compare with `niri-rpc--ensure-connected', which
errors when not connected."
  (and niri-rpc--async-process
       (eq (process-status niri-rpc--async-process) 'open)))
```

- [ ] **Step 4: Run the test to verify it passes**

Run:
```bash
cd /home/binarin/personal-workspace/emacs-niri-awareness && ./run-unit-tests.sh
```
Expected: all tests PASS including `niri-rpc-connected-p-disconnected`.

- [ ] **Step 5: Commit**

```bash
cd /home/binarin/personal-workspace/emacs-niri-awareness
git add niri-rpc.el niri-rpc-test.el run-unit-tests.sh
git commit -m "feat: add niri-rpc-connected-p predicate"
```

---

### Task 2: Cover the live-process branch of `niri-rpc-connected-p`

A second unit test confirms the non-nil branch without needing a real niri socket, by using a pipe process as a stand-in.

**Files:**
- Modify: `emacs-niri-awareness/niri-rpc-test.el` (append one test)
- Modify: `emacs-niri-awareness/run-unit-tests.sh` (add the test name to `rpc_unit`)

**Interfaces:**
- Consumes: `niri-rpc-connected-p` (Task 1).

- [ ] **Step 1: Write the unit test**

Append to `emacs-niri-awareness/niri-rpc-test.el`:

```elisp
(ert-deftest niri-rpc-connected-p-connected ()
  "niri-rpc-connected-p is non-nil for a process whose status is `open'.
Uses a pipe process as a stand-in so no niri socket is required."
  (let* ((proc (make-pipe-process :name "fake-niri"
                                  :buffer nil
                                  :noquery t))
         (niri-rpc--async-process proc))
    (unwind-protect
        (should (niri-rpc-connected-p))
      (when (process-live-p proc)
        (delete-process proc)))))
```

Add `niri-rpc-connected-p-connected` to the `rpc_unit` array in `run-unit-tests.sh`.

- [ ] **Step 2: Run the test to verify it passes**

Run:
```bash
cd /home/binarin/personal-workspace/emacs-niri-awareness && ./run-unit-tests.sh
```
Expected: PASS. (The predicate from Task 1 already satisfies this; the goal is to lock in the non-nil branch. If `make-pipe-process`'s signature differs across Emacs versions and the test fails on that, adjust the keyword args until the process is created with status `open`.)

- [ ] **Step 3: Commit**

```bash
cd /home/binarin/personal-workspace/emacs-niri-awareness
git add niri-rpc-test.el run-unit-tests.sh
git commit -m "test: cover niri-rpc-connected-p live-process branch"
```

---

### Task 3: Point `emacs-niri-awareness.url` at local checkout (uncommitted)

Enable `nixos-config` to consume the new predicate before it's published. **Do not commit this change** — revert in Task 7.

**Files:**
- Modify (uncommitted): `nixos-config/flake.nix`
- Modify (uncommitted): `nixos-config/flake.lock`

- [ ] **Step 1: Inspect the current input**

Run:
```bash
cd /home/binarin/personal-workspace/nixos-config && grep -n "emacs-niri-awareness" flake.nix
```
Expected: a line like `emacs-niri-awareness.url = "github:binarin/emacs-niri-awareness";`.

- [ ] **Step 2: Override to local path**

Edit `flake.nix`, change only the `emacs-niri-awareness.url` line to:

```nix
emacs-niri-awareness.url = "path:/home/binarin/personal-workspace/emacs-niri-awareness";
```

- [ ] **Step 3: Update the lock**

Run:
```bash
cd /home/binarin/personal-workspace/nixos-config && nix flake lock --update-input emacs-niri-awareness
```
Expected: `flake.lock` updated with a `path`-type entry for `emacs-niri-awareness`.

- [ ] **Step 4: Verify it builds**

Run:
```bash
cd /home/binarin/personal-workspace/nixos-config && nix build .#emacs --no-link
```
Expected: succeeds.

- [ ] **Step 5: Remember to revert before final commit**

Leave `flake.nix` and `flake.lock` modified in the working tree. Do **not** `git add` them in Tasks 4–6. Task 7 reverts `flake.nix` and relocks to the published source.

---

### Task 4: Add `b/other-window` command + repeat map

**Files:**
- Modify: `nixos-config/files/emacs/user-lisp/l-windows.el`
- Create: `nixos-config/files/emacs/tests/test-l-windows.el`

**Interfaces:**
- Consumes: `niri-rpc-connected-p` (Task 1), `niri-frame-niri-id`, `niri-rpc-focus-window`.
- Produces: `b/other-window`, `b/other-window-backward`, `b/other-window-repeat-map` (see Interfaces section).

**Testability note:** `selected-frame` is a C primitive and cannot be faked with `cl-letf`. The three unit tests below cover the fully testable logic: argument forwarding, `'visible` passing, the disconnected no-sync path, and the same-frame no-focus path. The different-frame → `FocusWindow` branch is verified manually in Task 6.

- [ ] **Step 1: Write the failing unit tests**

Create `nixos-config/files/emacs/tests/test-l-windows.el` with exactly this content (three tests, no placeholders):

```elisp
;;; test-l-windows.el --- Tests for l-windows.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'l-windows (expand-file-name "user-lisp/l-windows.el"
                                      (file-name-directory
                                       (or load-file-name buffer-file-name))))

(ert-deftest b/other-window-forwards-default-count-and-visible ()
  "b/other-window calls (other-window 1 'visible) by default."
  (cl-letf (((symbol-function 'niri-rpc-connected-p) (lambda () nil)))
    (should
     (equal
      (catch 'called-with
        (cl-letf (((symbol-function 'other-window)
                   (lambda (count all-frames)
                     (throw 'called-with (list count all-frames)))))
          (b/other-window)))
      (list 1 'visible)))))

(ert-deftest b/other-window-forwards-negative-count ()
  "b/other-window -1 forwards -1 to other-window, still with 'visible."
  (cl-letf (((symbol-function 'niri-rpc-connected-p) (lambda () nil)))
    (should
     (equal
      (catch 'called-with
        (cl-letf (((symbol-function 'other-window)
                   (lambda (count all-frames)
                     (throw 'called-with (list count all-frames)))))
          (b/other-window -1)))
      (list -1 'visible)))))

(ert-deftest b/other-window-no-focus-when-same-frame ()
  "When connected but other-window doesn't change frame, no FocusWindow."
  (let (calls)
    (cl-letf (((symbol-function 'niri-rpc-connected-p) (lambda () t))
              ;; Stub other-window to be a no-op: selected-frame is unchanged.
              ((symbol-function 'other-window)
               (lambda (&rest _) nil))
              ((symbol-function 'niri-rpc-focus-window)
               (lambda (id) (push id calls))))
      (b/other-window))
    (should (null calls))))
```

- [ ] **Step 2: Run the tests to verify they fail**

`scripts/test-emacs.sh` uses `shopt -s globstar`, so `files/emacs/tests/**/*.el` is discovered automatically — no list to update.

Run:
```bash
cd /home/binarin/personal-workspace/nixos-config && bash scripts/test-emacs.sh
```
Expected: FAIL — `b/other-window` is void (not yet defined in `l-windows.el`).

- [ ] **Step 3: Implement the commands and repeat map**

First, add `declare-function` forms for the three niri functions, immediately after the `;; -*- lexical-binding: t; -*-` line at the top of `nixos-config/files/emacs/user-lisp/l-windows.el`:

```elisp
(declare-function niri-rpc-connected-p "niri-rpc")
(declare-function niri-frame-niri-id "niri-frame")
(declare-function niri-rpc-focus-window "niri-rpc")
```

Then, before the final `(provide 'l-windows)` at the end of the file, append:

```elisp
;;;###autoload
(defun b/other-window-backward (&optional count)
  "Like `b/other-window', but move in the opposite direction.
COUNT defaults to 1; pass it to `b/other-window' negated."
  (interactive "p")
  (b/other-window (- (or count 1))))

(defvar-keymap b/other-window-repeat-map
  :doc "Repeat map for `b/other-window'.  Used in `repeat-mode'."
  :repeat t
  "o" #'b/other-window
  "O" #'b/other-window-backward)

;;;###autoload
(defun b/other-window (&optional count)
  "Select another window, cycling visible windows only.
Calls `other-window' with COUNT (default 1) and the symbol
`visible' as ALL-FRAMES, so minimized/iconified frames are
skipped.  When the niri IPC connection is live
\(`niri-rpc-connected-p') and the selected frame changed as a
result, sync Wayland keyboard focus to the new frame by sending
a `FocusWindow' action for its niri window id (if known).
No focus sync is attempted when the frame did not change or
when `niri-frame-niri-id' returns nil."
  (interactive "p")
  (let ((before-frame (selected-frame)))
    (other-window (or count 1) 'visible)
    (when (and (niri-rpc-connected-p)
               (not (eq before-frame (selected-frame))))
      (when-let* ((id (niri-frame-niri-id (selected-frame))))
        (niri-rpc-focus-window id)))))

(put 'b/other-window 'repeat-map 'b/other-window-repeat-map)
(put 'b/other-window-backward 'repeat-map 'b/other-window-repeat-map)
```

- [ ] **Step 4: Run the tests to verify they pass**

Run:
```bash
cd /home/binarin/personal-workspace/nixos-config && bash scripts/test-emacs.sh
```
Expected: PASS — byte-compilation clean (no warnings), all three ERT tests pass.

- [ ] **Step 5: Commit**

```bash
cd /home/binarin/personal-workspace/nixos-config
git add files/emacs/user-lisp/l-windows.el files/emacs/tests/test-l-windows.el
git commit -m "feat(emacs): b/other-window syncs niri focus on frame change"
```

---

### Task 5: Bind `C-x o` to `b/other-window` in `init.el`

**Files:**
- Modify: `nixos-config/files/emacs/init.el`

**Interfaces:**
- Consumes: `b/other-window` (Task 4).

- [ ] **Step 1: Add the binding**

In `files/emacs/init.el`, add near the other top-level `bind-key` / keymap setup (after the `repeat-mode` `use-package` block around line 138). Insert a single line:

```elisp
(bind-key [remap other-window] #'b/other-window)
```

`bind-key` is already available in this config. `[remap other-window]` routes the default `C-x o` (and any other key bound to `other-window`) through `b/other-window`; direct Lisp `(other-window …)` calls are unaffected by remapping.

- [ ] **Step 2: Byte-compile to verify no warnings**

Run:
```bash
cd /home/binarin/personal-workspace/nixos-config && bash scripts/test-emacs.sh
```
Expected: PASS — `b/other-window` is autoloaded so no `require` is needed; no byte-compile warnings.

- [ ] **Step 3: Commit**

```bash
cd /home/binarin/personal-workspace/nixos-config
git add files/emacs/init.el
git commit -m "feat(emacs): bind C-x o to b/other-window via remap"
```

---

### Task 6: Manual verification under niri (no automation)

**Files:** none.

- [ ] **Step 1: Rebuild Emacs**

```bash
cd /home/binarin/personal-workspace/nixos-config && nix build .#emacs --no-link
```
Switch/reload so the running Emacs picks up the new config (rebuild the system/home profile and restart the Emacs daemon, or `nix run` the new emacs).

- [ ] **Step 2: Verify same-frame navigation**

In an Emacs with two windows in one frame, press `C-x o`. Expected: point moves to the other window, no frame change, no visible niri focus action, typing lands in the new window. No error in `*Messages*`.

- [ ] **Step 3: Verify cross-frame navigation**

Open two Emacs frames on different niri windows/columns. From frame A, `C-x o` until focus reaches a window in frame B. Expected: Wayland keyboard focus follows — typing immediately lands in frame B without snapping back.

- [ ] **Step 4: Verify repeat chaining**

From frame A, press `C-x o` then `o` (no prefix). Expected: repeat works, each `o` moves to the next visible window and, on frame change, syncs niri focus. Press `O` to go backwards.

- [ ] **Step 5: Verify `'visible` semantics**

Iconify/minimize a frame (or move it to a non-visible workspace). `C-x o` should skip it.

- [ ] **Step 6: Verify disconnected graceful behavior**

Evaluate `(niri-rpc-disconnect)` in `*scratch*`. `C-x o` should still move between windows normally (just without Wayland focus sync). No error.

---

### Task 7: Publish the library and revert the flake override

**Files:**
- Modify: `nixos-config/flake.nix` (revert the uncommitted override from Task 3)
- Modify: `nixos-config/flake.lock` (relock to the published source)

- [ ] **Step 1: Publish `emacs-niri-awareness`**

Push the `niri-rpc-connected-p` commit (Task 1) — and the live-process test (Task 2) — from `emacs-niri-awareness` to the `github:binarin/emacs-niri-awareness` remote (master).

- [ ] **Step 2: Revert the local path override**

In `nixos-config/flake.nix`, change the `emacs-niri-awareness.url` line back to:

```nix
emacs-niri-awareness.url = "github:binarin/emacs-niri-awareness";
```

- [ ] **Step 3: Re-lock to the published revision**

Run:
```bash
cd /home/binarin/personal-workspace/nixos-config && nix flake lock --update-input emacs-niri-awareness
```
Confirm the locked `rev` matches the commit pushed in Step 1.

- [ ] **Step 4: Final build**

Run:
```bash
cd /home/binarin/personal-workspace/nixos-config && nix build .#emacs --no-link
```
Expected: succeeds, pulling the published library.

- [ ] **Step 5: Commit the lock update**

If `flake.nix` shows no net diff (fully reverted), only `flake.lock` is committed:

```bash
cd /home/binarin/personal-workspace/nixos-config
git add flake.lock
git commit -m "chore: relock emacs-niri-awareness for niri-rpc-connected-p"
```

(If `flake.nix` retained any unrelated local diff, include it explicitly; otherwise leave `flake.nix` untouched.)

---

## Self-Review

- **Spec coverage:** predicate (Tasks 1–2) ✓; `b/other-window` with `'visible` + frame-change guard + `niri-rpc-connected-p` gate (Task 4) ✓; own repeat map routing through the wrapper, with a named reverse command (Task 4) ✓; autoload + `[remap other-window]` binding in `init.el` (Tasks 4–5) ✓; flake override for testing (Task 3) and revert (Task 7) ✓.
- **Type/name consistency:** `b/other-window`, `b/other-window-backward`, `b/other-window-repeat-map`, `niri-rpc-connected-p` used consistently across all tasks and in the Interfaces section.
- **Placeholder scan:** Task 4 contains exactly three real unit tests (no `ert-skip`, no placeholder text, no "TBD"). The frame-change → FocusWindow path is honestly documented as manual-only (Task 6) because `selected-frame` is a C primitive that `cl-letf` cannot fake. All code blocks are complete and copy-pasteable.
