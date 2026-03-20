#!/usr/bin/env bash
nix run .#emacs-clean-nox -- -Q			\
    -L ./files/emacs/lisp			\
    --batch					\
    -l ./files/emacs/tests/config-test-pre.el	\
    -l ./files/emacs/init.el			\
    -l ./files/emacs/tests/config-tests.el	\
    -f ert-run-tests-batch-and-exit
