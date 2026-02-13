#!/usr/bin/env bash
# Find the first available (commented-out) IP in a network file
# Usage: ./find-free-ip.sh [network]
# Default network: home
#
# Parses inventory/networks/<network>.nix and finds the first commented IP

set -euo pipefail

network="${1:-home}"
network_file="inventory/networks/${network}.nix"

if [[ ! -f "$network_file" ]]; then
    echo "Error: Network file not found: $network_file" >&2
    exit 1
fi

# Find the first commented IP address line
# Pattern: # "192.168.x.y" = "";
grep -E '^\s*#\s*"192\.168\.[0-9]+\.[0-9]+"' "$network_file" | head -1 | \
    perl -nE 'm/"(192\.168\.[0-9]+\.[0-9]+)"/ && say $1'
