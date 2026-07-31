# Emacs Configuration

## Testing

Run `scripts/test-emacs.sh` to byte-compile all `.el` files and run ERT tests:

```bash
bash scripts/test-emacs.sh
```

It uses `nix develop .#emacs-test` with bubblewrap sandboxing. Byte-compilation runs with
`byte-compile-error-on-warn t`, so **any warning is a failure** — all functions must be declared.

## Peculiarities

### Autoloads

`.user-lisp-autoloads.el` is **auto-generated** (via `loaddefs-generate`) as part of the build
process. Do not commit it (it's gitignored). Do not manually regenerate it — any function
marked with `;;;###autoload` will get its autoload cookie during the next build.

### Byte-compiler strictness

Because `byte-compile-error-on-warn` is set to `t`, the byte-compiler treats warnings as
errors. Common issues:

- **Unknown functions**: If you call a function from another package (e.g. `magit-file-delete`,
  `dired-get-marked-files`, `project-root`), add either:
  - A `declare-function` form: `(declare-function magit-file-delete "magit-files")`
  - A `require` for the package: `(require 'project)`
  - Add it to the `:commands` list in `use-package`: `:commands (magit-file-delete)`

- **Obsolete macros**: Emacs 31.1 marks `if-let` as obsolete — use `if-let*` instead.

### File structure

| File | Purpose |
|---|---|
| `files/emacs/init.el` | Main init, keybindings, use-package declarations |
| `files/emacs/early-init.el` | Early startup (frame, package, GC settings) |
| `files/emacs/user-lisp/b-*.el` | Feature modules (files, version-control, org, etc.) |
| `files/emacs/user-lisp/l-*.el` | Library/utility modules |
| `files/emacs/user-lisp/.user-lisp-autoloads.el` | Auto-generated autoloads (do not edit, gitignored) |
| `files/emacs/tests/` | ERT tests |

### Project keymap bindings (`C-x p`)

Bindings go in `init.el` inside the `use-package b-ripgrep` block's `:bind` form (alongside
`C-x p r`). Commands called from project bindings must be defined with `;;;###autoload` in
their respective `b-*.el` files.

### Dired customizations

Dired keybindings are set in `b/dired-mode-hook` via `keymap-local-set`. All capital
letters A–Z are taken by dired defaults — use modifier keys like `M-k`.
