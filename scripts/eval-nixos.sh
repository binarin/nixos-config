#!/usr/bin/env bash
set -euo pipefail

# Evaluate a NixOS configuration (get the .drv file without building)
# Usage: eval-nixos.sh <configuration> [nix-options...]
#
# Arguments:
#   configuration: The NixOS configuration to evaluate (e.g., hostname)
#   nix-options:   Additional options to pass to nix eval (optional)
#
# This script evaluates the configuration and prints the .drv path,
# ensuring all dependencies are instantiated but not built.
#
# Primary use case: Debugging infinite recursion and evaluation errors.
# When `nix flake check` fails with infinite recursion, error diagnostics
# are often insufficient to pinpoint the problem location. This script
# helps isolate which configuration is problematic.

if [ $# -lt 1 ]; then
    echo "Usage: $0 <configuration> [nix-options...]" >&2
    exit 1
fi

configuration="$1"
shift
nix_options=("$@")

repo_root="$(pwd)"

# Evaluate and get the derivation path
drv_path=$(nix eval --raw \
    "${repo_root}#nixosConfigurations.${configuration}.config.system.build.toplevel.drvPath" \
    "${nix_options[@]}")

echo "Configuration evaluated successfully!"
echo "Derivation path: ${drv_path}"

# Also show the derivation to verify it's complete
echo ""
echo "Derivation details:"
nix derivation show "${drv_path}" "${nix_options[@]}"
