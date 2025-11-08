#!/usr/bin/env bash
set -euo pipefail

# Build a NixOS configuration
# Usage: build-nixos.sh <configuration> <output-path> [nix-options...]
#
# Arguments:
#   configuration: The NixOS configuration to build (e.g., hostname)
#   output-path:   Where to store the result symlink
#   nix-options:   Additional options to pass to nix build (optional)

if [ $# -lt 2 ]; then
    echo "Usage: $0 <configuration> <output-path> [nix-options...]" >&2
    exit 1
fi

configuration="$1"
output_path="$2"
shift 2
nix_options=("$@")

repo_root="$(pwd)"

# Build the NixOS configuration
nix build \
    "${repo_root}#nixosConfigurations.${configuration}.config.system.build.toplevel" \
    --keep-going \
    -o "${output_path}" \
    "${nix_options[@]}"
