#!/usr/bin/env bash
set -x
nix run .#emacs-clean-nox -- \
    -Q \
    -L ./files/emacs/ \
    --batch \
    --eval '(setf byte-compile-error-on-warn t)' \
    --eval '(message "%s" load-path)' \
    --eval '(unless (byte-compile-file "'"$1"'") (kill-emacs 1))'
