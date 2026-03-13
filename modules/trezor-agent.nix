{
  inputs,
  ...
}:
let
  mkTrezorAgentPkgs =
    system:
    import inputs.nixpkgs-trezor-agent {
      inherit system;
      config = {
        allowUnfree = true;
        permittedInsecurePackages = [
          # used by trezor-agent, but vulnerability is about leaking
          # generated keys - so doesn't matter, as keys do not leave
          # trezor
          "python3.12-ecdsa-0.19.1"
          "python3.13-ecdsa-0.19.1"
        ];
      };
    };
in
{
  flake-file.inputs.nixpkgs-trezor-agent = {
    url = "github:nixos/nixpkgs?rev=41e216c0ca66c83b12ab7a98cc326b5db01db646";
  };

  # Expose trezor-agent as a package so CI can test building it
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

  flake.homeModules.trezor-agent =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.trezor-agent";
      home.packages = [
        (mkTrezorAgentPkgs pkgs.stdenv.hostPlatform.system).trezor-agent
      ];
    };
}
