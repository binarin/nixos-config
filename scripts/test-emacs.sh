#!/usr/bin/env bash
set -euo pipefail

shopt -s globstar # ** in globs

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

FAKE_HOME="$(mktemp -d)"
TEMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TEMP_DIR"' EXIT

pushd "$TEMP_DIR" > /dev/null 2>&1
cp -sr $REPO_DIR/files/emacs/{init.el,early-init.el} .
for dir in user-lisp tests; do
    mkdir $TEMP_DIR/$dir
    pushd $TEMP_DIR/$dir > /dev/null 2>&1
    cp -sr $REPO_DIR/files/emacs/$dir/*.el .
    popd > /dev/null 2>&1
done
popd > /dev/null 2>&1

BWRAP=$(nix build '.#nixosConfigurations.nix-builder-raum.pkgs.bubblewrap' --no-link --print-out-paths)/bin/bwrap
EMACS=$(nix build '.#emacs' --no-link --print-out-paths)/bin/emacs

run_emacs() {
    "$BWRAP"									\
	--unshare-all								\
	--tmpfs /								\
	--ro-bind /nix/store /nix/store						\
	--ro-bind /bin/sh /bin/sh						\
	--ro-bind "$REPO_DIR" "$REPO_DIR"					\
	--bind "$TEMP_DIR" "$TEMP_DIR"						\
	--bind "$FAKE_HOME" "$FAKE_HOME"					\
	--setenv HOME "$FAKE_HOME"						\
	--setenv XDG_STATE_HOME "$FAKE_HOME/.local/state"			\
	--setenv XDG_CACHE_HOME "$FAKE_HOME/.cache"				\
	--setenv XDG_RUNTIME_DIR "$FAKE_HOME/.run"				\
	--dev /dev								\
	--chdir /								\
	--bind /proc /proc							\
	--uid $(id -u)								\
	-- "$EMACS"								\
	--init-directory "$TEMP_DIR"						\
	--directory "$TEMP_DIR/user-lisp"					\
	--eval '(startup-redirect-eln-cache (locate-user-emacs-file "eln"))'	\
	"$@"
}

failed=0

for f in "$TEMP_DIR"/**/*.el; do
    early_init_array=()
    if [[ $(basename $f) != "early-init.el" ]]; then
	early_init_array=(--load $TEMP_DIR/early-init.el)
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
    -l $TEMP_DIR/early-init.el \
    -l $TEMP_DIR/tests/config-tests.el	\
    -f ert-run-tests-batch-and-exit \
    || { failed=1; }

if [ "$failed" -ne 0 ]; then
    echo "Some files failed to compile"
    exit 1
fi

echo "All files compiled successfully"
