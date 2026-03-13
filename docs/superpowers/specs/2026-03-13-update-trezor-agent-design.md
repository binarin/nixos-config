# Design: Extract trezor-agent upgrade logic into a flake package

## Problem

The trezor-agent upgrade logic currently lives inline in the Forgejo CI
workflow (`.forgejo/workflows/flake-update.yaml`, lines 78-123). It uses
`sed`/`grep`/`jq` to update the nixpkgs rev in `modules/trezor-agent.nix`,
then attempts a build. Two issues:

1. **No automatic insecure package handling.** When a Python version change
   introduces a new `ecdsa` package name (e.g. `python3.14-ecdsa-0.19.1`),
   the build fails and requires manual intervention to add it to
   `permittedInsecurePackages`.
2. **Logic is embedded in CI YAML**, making it hard to test or run locally.

## Solution

Create a `packages.update-trezor-agent` flake package defined inline in
`modules/trezor-agent.nix` using `writeShellApplication`. Simplify the CI
job to call `nix run .#update-trezor-agent`.

## Script behavior

### Input

- Optional `--rev <REV>` argument specifying the target nixpkgs revision.
- If not provided, extracts the main nixpkgs rev from `flake.lock` via `jq`.

### Steps

1. **Determine target rev.** Parse `--rev` argument or read from
   `flake.lock`: `jq -r '.nodes.nixpkgs.locked.rev' flake.lock`.
2. **Read current rev** from `modules/trezor-agent.nix` via `grep -oP
   '(?<=\?rev=)[a-f0-9]+'`.
3. **Compare.** If revisions match, print message and exit 0.
4. **Update rev** in `modules/trezor-agent.nix` via `sed -i
   "s|?rev=$OLD_REV|?rev=$NEW_REV|"`.
5. **Regenerate flake.nix**: `nix run '.#write-flake'`. No separate
   `nix flake lock` is needed — the pinned `?rev=` URL auto-resolves on
   build.
6. **First build attempt**: `nix build '.#trezor-agent' --no-link 2>&1`.
   - On success: exit 0.
   - On failure: check if error mentions insecure `ecdsa` package(s). If
     not, leave changes and exit 1.
7. **Extract all insecure package names** from the build error output. Nix
   may report multiple insecure packages simultaneously (e.g., when
   multiple Python versions each have their own `ecdsa` derivation).
   Extract all of them.
8. **Append each to `permittedInsecurePackages`** list in
   `modules/trezor-agent.nix`. Insert new `"<package-name>"` lines before
   the closing `];` of the list. Accumulate — do not remove old entries.
9. **Second build attempt**: `nix build '.#trezor-agent' --no-link`.
    - On success: exit 0.
    - On failure: leave all changes in place, exit 1.

### Error output parsing

Nix reports insecure package errors with a message like:
```
error: Package 'python3.14-ecdsa-0.19.1' in ... is marked as insecure
```

The script extracts the package name from this pattern using grep/sed.

## Packaging

Defined in `modules/trezor-agent.nix` inside the existing `perSystem` block:

```nix
perSystem = { system, pkgs, ... }: {
  packages.trezor-agent = (mkTrezorAgentPkgs system).trezor-agent;
  packages.update-trezor-agent = pkgs.writeShellApplication {
    name = "update-trezor-agent";
    runtimeInputs = [ pkgs.jq pkgs.gnused pkgs.gnugrep pkgs.coreutils ];
    text = ''
      # ... script body
    '';
  };
};
```

Using `writeShellApplication` which provides `set -euo pipefail` and
shellcheck validation, plus bundles runtime deps via PATH.

## CI workflow change

The `update-trezor-nixpkgs` job in `.forgejo/workflows/flake-update.yaml`
simplifies from ~45 lines to:

```yaml
update-trezor-nixpkgs:
  runs-on: native
  needs: update-caddy-hash
  continue-on-error: true
  steps:
    - uses: actions/checkout@v4
      with:
        ref: flake-bump
    - uses: ./.forgejo/actions/git-crypt-unlock
      with:
        git-crypt-key: ${{ secrets.GIT_CRYPT_KEY }}
    - name: Update nixpkgs-trezor-agent to match main nixpkgs
      run: nix run .#update-trezor-agent
    - name: Commit and push if build succeeded
      run: |
        if git diff --quiet; then
          echo "No changes to commit"
          exit 0
        fi
        git config user.name "Automatic Flake Updater"
        git config user.email "flake-updater@binarin.info"
        git add -A
        git commit -m "Bump nixpkgs-trezor-agent to match nixpkgs"
        git push --force origin HEAD:flake-bump
```

The commit/push logic stays in CI as it's CI-specific.

## Files changed

1. **`modules/trezor-agent.nix`** — add `packages.update-trezor-agent` in
   `perSystem` block.
2. **`.forgejo/workflows/flake-update.yaml`** — simplify
   `update-trezor-nixpkgs` job to use `nix run .#update-trezor-agent`.
