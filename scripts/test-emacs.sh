#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar # ** in globs

if [[ ! -v IN_TEST_SHELL ]]; then
    exec nix develop .#emacs-test --ignore-env --command "$0" "$@"
fi



SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
EMACS_DIR="$REPO_DIR/files/emacs"

FAKE_HOME="$(mktemp -d)"
TEMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TEMP_DIR" "$FAKE_HOME"' EXIT


rsync -a --exclude='*.elc' $EMACS_DIR/ $TEMP_DIR

# BWRAP=$(nix build '.#nixosConfigurations.nix-builder-raum.pkgs.bubblewrap' --no-link --print-out-paths)/bin/bwrap
# EMACS=$(nix build '.#emacs' --no-link --print-out-paths)/bin/emacs
BWRAP=bwrap
EMACS=emacs

run_emacs() {
    "$BWRAP"									\
	--unshare-all								\
	--tmpfs /								\
	--ro-bind /nix/store /nix/store						\
	--ro-bind /bin/sh /bin/sh						\
	--ro-bind "$REPO_DIR" "$REPO_DIR"					\
	--bind "$FAKE_HOME" "$FAKE_HOME"					\
	--bind "$TEMP_DIR" "$EMACS_DIR"						\
	--setenv HOME "$FAKE_HOME"						\
	--setenv XDG_STATE_HOME "$FAKE_HOME/.local/state"			\
	--setenv XDG_CACHE_HOME "$FAKE_HOME/.cache"				\
	--setenv XDG_RUNTIME_DIR "$FAKE_HOME/.run"				\
	--dev /dev								\
	--bind /proc /proc							\
	--uid $(id -u)								\
	--chdir "$REPO_DIR"							\
	-- "$EMACS"								\
	--init-directory "$EMACS_DIR"						\
	--directory "$EMACS_DIR/user-lisp"					\
	--eval '(startup-redirect-eln-cache (locate-user-emacs-file "eln"))'	\
	"$@" \
	2> >(grep -vP '^Loading.*site-lisp/site-start' >&2)
}




failed=0

for f in "$EMACS_DIR"/**/*.el; do
    f=${f#$REPO_DIR/}
    early_init_array=()
    if [[ $(basename $f) != "early-init.el" ]]; then
	early_init_array=(--load $EMACS_DIR/early-init.el)
    fi
    printf "Compiling: \x1b[;32m%s\x1b[0m\n" "$f"
    run_emacs \
         --batch \
	 "${early_init_array[@]}" \
         --eval '(setf byte-compile-error-on-warn t)' \
         --eval "(unless (byte-compile-file \"$f\") (message \"Failed to compile: %s\" \"$f\") (kill-emacs 1))" \
        || { failed=1; echo "FAILED: $f"; }
done

find "$TEMP_DIR" -name '*.elc' -delete

run_emacs \
    --batch \
    -l $EMACS_DIR/early-init.el \
    -l $EMACS_DIR/tests/config-tests.el	\
    -f ert-run-tests-batch-and-exit \
    || { failed=1; }

if [ "$failed" -ne 0 ]; then
    echo "Some files failed to compile"
    exit 1
fi

echo "All files compiled successfully"
