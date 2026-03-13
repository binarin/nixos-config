# Update Trezor-Agent Script Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract trezor-agent upgrade logic from CI YAML into a standalone flake package with automatic insecure ecdsa handling.

**Architecture:** A shell script packaged via `writeShellApplication` in the existing `modules/trezor-agent.nix` dendritic module. The script updates the nixpkgs rev, regenerates the flake, attempts a build, and auto-adds insecure ecdsa exceptions on failure. CI workflow simplified to `nix run .#update-trezor-agent`.

**Tech Stack:** Nix (writeShellApplication), bash, jq, sed, grep

**Spec:** `docs/superpowers/specs/2026-03-13-update-trezor-agent-design.md`

---

## Chunk 1: Script and CI changes

### Task 1: Fix typo and add update-trezor-agent package to modules/trezor-agent.nix

**Files:**
- Modify: `modules/trezor-agent.nix`

- [ ] **Step 1: Fix the typo on line 16**

Line 16 has `"python3.12-ecdsa;-0.19.1"` — the semicolon before the dash is a typo. Fix it to `"python3.12-ecdsa-0.19.1"`.

- [ ] **Step 2: Add `packages.update-trezor-agent` to the `perSystem` block**

Add `pkgs` to the `perSystem` argument set (currently only has `system`). Add the new package. The full updated `perSystem` block should be:

```nix
  perSystem =
    { system, pkgs, ... }:
    {
      packages.trezor-agent = (mkTrezorAgentPkgs system).trezor-agent;
      packages.update-trezor-agent = pkgs.writeShellApplication {
        name = "update-trezor-agent";
        runtimeInputs = with pkgs; [
          jq
          gnused
          gnugrep
          coreutils
        ];
        text = ''
          # Must run from repo root
          if [[ ! -f flake.lock ]]; then
            echo "Error: must run from repository root (flake.lock not found)" >&2
            exit 1
          fi

          TREZOR_FILE="modules/trezor-agent.nix"

          # Parse --rev argument or default to flake.lock nixpkgs rev
          NEW_REV=""
          while [[ $# -gt 0 ]]; do
            case "$1" in
              --rev)
                if [[ $# -lt 2 ]]; then
                  echo "Error: --rev requires a value" >&2
                  exit 1
                fi
                NEW_REV="$2"
                shift 2
                ;;
              *)
                echo "Unknown argument: $1" >&2
                exit 1
                ;;
            esac
          done

          if [[ -z "$NEW_REV" ]]; then
            NEW_REV=$(jq -r '.nodes.nixpkgs.locked.rev' flake.lock)
          fi
          echo "Target nixpkgs rev: $NEW_REV" >&2

          # Read current rev from trezor-agent module (first match only)
          OLD_REV=$(grep -m1 -o '?rev=[a-f0-9]*' "$TREZOR_FILE" | sed 's/?rev=//')
          echo "Current trezor nixpkgs rev: $OLD_REV" >&2

          if [[ "$NEW_REV" == "$OLD_REV" ]]; then
            echo "Revisions match, nothing to do" >&2
            exit 0
          fi

          # Update rev
          sed -i "s|?rev=$OLD_REV|?rev=$NEW_REV|" "$TREZOR_FILE"
          echo "Updated $TREZOR_FILE with new rev" >&2

          # Regenerate flake.nix
          nix run '.#write-flake'

          # First build attempt
          BUILD_OUTPUT=""
          if BUILD_OUTPUT=$(nix build '.#trezor-agent' --no-link 2>&1); then
            echo "Build succeeded" >&2
            exit 0
          fi

          echo "First build failed, checking for insecure package errors..." >&2
          echo "$BUILD_OUTPUT" >&2

          # Extract insecure ecdsa package names from error output
          # Nix error format: Package 'python3.14-ecdsa-0.19.1' in ... is marked as insecure
          INSECURE_PKGS=$(echo "$BUILD_OUTPUT" | grep -o "Package '[^']*ecdsa[^']*'" | sed "s/Package '//;s/'$//" || true)

          if [[ -z "$INSECURE_PKGS" ]]; then
            echo "Build failed for reasons other than insecure ecdsa packages" >&2
            exit 1
          fi

          echo "Found insecure ecdsa packages:" >&2
          echo "$INSECURE_PKGS" >&2

          # Add each insecure package to permittedInsecurePackages list
          while IFS= read -r pkg; do
            # Check if already in the list
            if grep -qF "\"$pkg\"" "$TREZOR_FILE"; then
              echo "Package $pkg already in exceptions list, skipping" >&2
              continue
            fi
            # Insert before the closing ]; of permittedInsecurePackages
            # Uses a line number approach: find ]; line within the block, insert before it
            LINE_NUM=$(sed -n '/permittedInsecurePackages/,/];/{ /];/= }' "$TREZOR_FILE")
            sed -i "$LINE_NUM i\\          \"$pkg\"" "$TREZOR_FILE"
            echo "Added $pkg to insecure exceptions" >&2
          done <<< "$INSECURE_PKGS"

          # Second build attempt
          if nix build '.#trezor-agent' --no-link; then
            echo "Build succeeded after adding insecure exceptions" >&2
            exit 0
          else
            echo "Build still fails after adding insecure exceptions. Changes left in place." >&2
            exit 1
          fi
        '';
      };
    };
```

- [ ] **Step 3: Verify the module evaluates**

Run: `ncf eval nixos`
Expected: Successful evaluation (no errors about the new package definition).

- [ ] **Step 4: Verify the package builds**

Run: `nix build .#update-trezor-agent --no-link`
Expected: Successful build. This validates the `writeShellApplication` (including shellcheck).

- [ ] **Step 5: Format**

Run: `nix fmt`

- [ ] **Step 6: Commit**

```bash
git add modules/trezor-agent.nix
git commit -m "feat(trezor-agent): add update-trezor-agent package

Extracts upgrade logic into a writeShellApplication package that:
- Updates nixpkgs-trezor-agent rev (from flake.lock or --rev)
- Auto-detects and adds insecure ecdsa exceptions on build failure
- Retries build after adding exceptions

Also fixes typo in existing permittedInsecurePackages entry."
```

### Task 2: Simplify CI workflow

**Files:**
- Modify: `.forgejo/workflows/flake-update.yaml:78-123`

- [ ] **Step 1: Replace the `update-trezor-nixpkgs` job body**

Replace lines 89-112 (the two run steps "Update nixpkgs-trezor-agent to match main nixpkgs" and "Try building trezor-agent") with a single step:

```yaml
      - name: Update nixpkgs-trezor-agent to match main nixpkgs
        run: nix run .#update-trezor-agent
```

Keep the checkout, git-crypt-unlock, and commit/push steps unchanged. The full job should be:

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

- [ ] **Step 2: Commit**

```bash
git add .forgejo/workflows/flake-update.yaml
git commit -m "ci: simplify trezor-agent update to use nix run .#update-trezor-agent"
```
