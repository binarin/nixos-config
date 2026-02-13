#!/usr/bin/env bash
# Get the default stateVersion from an existing nixosConfiguration
# Usage: ./get-state-version.sh <machine-name>
#
# Run this AFTER creating a machine config without explicit stateVersion
# to discover the default value, then add it explicitly to the config.

set -euo pipefail

machine="${1:?Usage: $0 <machine-name>}"

nix eval --raw ".#nixosConfigurations.${machine}.config.system.stateVersion"
echo  # Add newline for cleaner output
