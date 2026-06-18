#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 [--remote-build] <hostname>"
  echo ""
  echo "Bootstrap nix + system-manager on a bentos (CentOS/RHEL) host."
  echo ""
  echo "Options:"
  echo "  --remote-build    Build the system-manager closure on the target host"
  echo "                    (default: build locally and copy)"
  exit 1
}

REMOTE_BUILD=false
HOST=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --remote-build) REMOTE_BUILD=true; shift ;;
    -h|--help) usage ;;
    -*) echo "Unknown option: $1"; usage ;;
    *) HOST="$1"; shift ;;
  esac
done

[[ -z "$HOST" ]] && usage

SSH_USER="allebedev"
NIX_BIN="/nix/var/nix/profiles/default/bin/nix"
NIX_DAEMON_SH="/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
SM_PROFILE="/nix/var/nix/profiles/system-manager-profiles/system-manager"

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

echo "==> Checking if nix is installed on $HOST..."
if ssh "$SSH_USER@$HOST" "test -x $NIX_BIN"; then
  echo "    nix already installed."
else
  echo "==> Installing nix on $HOST..."
  ssh "$SSH_USER@$HOST" "curl -fsSL https://install.determinate.systems/nix | sudo sh -s -- install --no-confirm --nix-build-user-id-base 666000 --nix-build-group-id 666000 --extra-conf 'trusted-users = allebedev root 15008352'"
  echo "    nix installed."
fi

FLAKE_REF="$REPO_ROOT#systemConfigs.b-db-k"

if [[ "$REMOTE_BUILD" == "true" ]]; then
  echo "==> Building system-manager config on $HOST..."
  STORE_PATH=$(ssh "$SSH_USER@$HOST" "source $NIX_DAEMON_SH && nix build --no-link --print-out-paths '$FLAKE_REF'")
else
  echo "==> Building system-manager config locally..."
  STORE_PATH=$(nix build --no-link --print-out-paths "$FLAKE_REF")

  echo "==> Copying closure to $HOST..."
  nix copy --to "ssh://$SSH_USER@$HOST" "$STORE_PATH"
fi

echo "==> Activating system-manager on $HOST (store path: $STORE_PATH)..."
ssh -t "$SSH_USER@$HOST" "sudo bash -c '
  source $NIX_DAEMON_SH
  mkdir -p \$(dirname $SM_PROFILE)
  $NIX_BIN-env --profile $SM_PROFILE --set $STORE_PATH
  $STORE_PATH/bin/activate
'"

echo "==> Done. system-manager activated on $HOST."
echo "    Future deploys: deploy .#b-db-k -s"
