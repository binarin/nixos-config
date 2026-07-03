let
  packageFn =
    { writeShellApplication, perl, git }:
    writeShellApplication {
      name = "patchcritic";
      runtimeInputs = [ perl git ];
      text = ''
        set -euo pipefail

        PATCHCRITIC_DIR="/usr/local/git_tree/patchcritic"

        usage() {
          echo "Usage: patchcritic [REV-RANGE|N]"
          echo ""
          echo "Run patchcritic on a git revision range."
          echo ""
          echo "Arguments:"
          echo "  REV-RANGE    A git revision range (e.g. HEAD~3..HEAD)"
          echo "  N            Number of commits back from HEAD (converted to HEAD~N..HEAD)"
          echo ""
          echo "Without arguments, walks first-parent chain from HEAD"
          echo "to the nearest merge commit and uses that as the diff base."
          echo "Errors if HEAD itself is a merge commit."
        }

        if [[ $# -gt 0 ]]; then
          if [[ "$1" =~ ^[0-9]+$ ]]; then
            # Number: convert to HEAD~N..HEAD
            revs="HEAD~$1..HEAD"
          elif [[ "$1" == *".."* ]]; then
            # Explicit revision range
            revs="$1"
          else
            case "$1" in
              -h|--help)
                usage
                exit 0
                ;;
              *)
                echo "Error: argument must be a revision range or a number, got: $1" >&2
                usage
                exit 1
                ;;
            esac
          fi
          shift
        else
          # Auto-detect: walk first-parent chain to nearest merge commit
          head_sha=$(git rev-parse HEAD)
          merge_base=$(git rev-list --first-parent --merges -n1 HEAD 2>/dev/null || true)

          if [[ -z "$merge_base" ]]; then
            echo "No merge commit found in first-parent history, falling back to HEAD~1..HEAD" >&2
            revs="HEAD~1..HEAD"
          elif [[ "$merge_base" == "$head_sha" ]]; then
            echo "Error: HEAD is a merge commit. Specify a rev range explicitly." >&2
            exit 1
          else
            revs="$merge_base..HEAD"
          fi
        fi

        if [[ ! -d "$PATCHCRITIC_DIR" ]]; then
          echo "Error: patchcritic not found at $PATCHCRITIC_DIR" >&2
          echo "Run 'bentos-setup git-patchcritic' to clone it first." >&2
          exit 1
        fi

        echo "Running patchcritic on revs=$revs..." >&2

        exec perl \
          -Ilib \
          -Idist/dev/bcritical/policies/lib \
          -I"$PATCHCRITIC_DIR/lib" \
          "$PATCHCRITIC_DIR/bin/patchcritic" \
          --profile=.perlcriticrc \
          --debug=diff \
          --revs="$revs" \
          "$@"
      '';
    };
in
{
  perSystem =
    { pkgs, ... }:
    {
      packages.bentos-patchcritic = pkgs.callPackage packageFn { };
    };

  flake.overlays.bentos-patchcritic = final: prev: {
    bentos-patchcritic = final.callPackage packageFn { };
  };
}
