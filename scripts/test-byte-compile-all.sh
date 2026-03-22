#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

EMACS=$(nix build '.#emacs' --no-link --print-out-paths)/bin/emacs

failed=0
for f in "$REPO_DIR"/files/emacs/**/*.el; do
    echo "Compiling: $f"
    "$EMACS" \
        -L "$REPO_DIR/files/emacs/lisp" \
        -l "$REPO_DIR/files/emacs/tests/config-test-pre.el" \
        --batch \
        --eval '(setf byte-compile-error-on-warn t)' \
        --eval "(unless (byte-compile-file \"$f\") (message \"Failed to compile: %s\" \"$f\") (kill-emacs 1))" \
        || { failed=1; echo "FAILED: $f"; }
done

if [ "$failed" -ne 0 ]; then
    echo "Some files failed to compile"
    exit 1
fi

echo "All files compiled successfully"
