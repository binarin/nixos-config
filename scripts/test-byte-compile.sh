#!/usr/bin/env bash
set -x
nix run .#emacs-clean-batch --				\
    -Q							\
    -L ./files/emacs/lisp				\
    --batch						\
    --eval '(setf byte-compile-error-on-warn t)'	\
    --eval '(unless (byte-compile-file "'"$1"'") (kill-emacs 1))'
