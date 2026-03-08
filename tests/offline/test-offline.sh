#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/../.."

echo "Building test container with pre-warmed nix store..."
podman build -f tests/offline/Dockerfile -t nixos-config-offline-test .

echo "Running offline tests (network disabled)..."
podman run --rm --network=none nixos-config-offline-test /bin/sh -c '
  set -e
  echo "=== Testing nix develop ==="
  nix develop --command echo "nix develop works"

  echo "=== Testing nix flake check ==="
  nix flake check

  echo "=== All offline tests passed! ==="
'
