#!/usr/bin/env bash

nix run .#emacs-clean-nox -- \
    -Q -L ./files/emacs/ --batch \
    --eval '(setf byte-compile-error-on-warn t)' \
    --eval '(unless (byte-compile-file "'"$1"'") (kill-emacs 1))'
