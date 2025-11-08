#!/usr/bin/env bash
set -euo pipefail

# Disable output buffering
exec 2>&1

# Evaluate all NixOS configurations and report success/failure
# Usage: eval-all-nixos.sh [nix-options...]
#
# Primary use case: Debugging infinite recursion and evaluation errors.
# When `nix flake check` fails, it evaluates all outputs together, making
# it difficult to identify which specific configuration is causing the problem.
# This script evaluates each configuration individually, isolating failures
# and providing clear diagnostics about which configs have issues.

# Handle Ctrl-C and other signals
trap 'echo -e "\n\nInterrupted by user"; exit 130' INT TERM

# ANSI colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BOLD='\033[1m'
RESET='\033[0m'

repo_root="$(pwd)"
nix_options=("$@")

# Get list of all nixosConfigurations
echo "Discovering NixOS configurations..."
if ! mapfile -t configs < <(nix flake show --json 2>/dev/null | jq -r '.nixosConfigurations | keys[]'); then
    echo -e "${RED}Error: Failed to discover nixosConfigurations${RESET}" >&2
    exit 1
fi

total=${#configs[@]}
if [ ${total} -eq 0 ]; then
    echo -e "${YELLOW}Warning: No nixosConfigurations found${RESET}"
    exit 0
fi

succeeded=0
failed=0
declare -a failed_configs

echo "Found ${total} configurations to evaluate"
echo ""

# Evaluate each configuration
start_time=$(date +%s)
current=0
total_eval_time=0

for config in "${configs[@]}"; do
    current=$((current + 1))

    # Print configuration name immediately
    printf "[%2d/%2d] %-30s " "${current}" "${total}" "${config}:"

    # Try to evaluate the configuration's derivation path
    config_start=$(date +%s)
    if nix eval --raw "${repo_root}#nixosConfigurations.${config}.config.system.build.toplevel.drvPath" "${nix_options[@]}" &>/dev/null; then
        config_end=$(date +%s)
        config_time=$((config_end - config_start))
        total_eval_time=$((total_eval_time + config_time))

        printf "${GREEN}✓ %-6s${RESET} (%3ds)" "OK" "${config_time}"
        succeeded=$((succeeded + 1))
    else
        config_end=$(date +%s)
        config_time=$((config_end - config_start))
        total_eval_time=$((total_eval_time + config_time))

        printf "${RED}✗ %-6s${RESET} (%3ds)" "FAILED" "${config_time}"
        failed=$((failed + 1))
        failed_configs+=("${config}")
    fi

    # Calculate and display ETA on the same line
    if [ ${current} -lt ${total} ]; then
        avg_time=$((total_eval_time / current))
        remaining=$((total - current))
        eta_seconds=$((avg_time * remaining))
        eta_min=$((eta_seconds / 60))
        eta_sec=$((eta_seconds % 60))
        printf " ${YELLOW}|${RESET} ETA: %dm%02ds\n" "${eta_min}" "${eta_sec}"
    else
        echo ""
    fi
done

end_time=$(date +%s)
total_time=$((end_time - start_time))

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
total_min=$((total_time / 60))
total_sec=$((total_time % 60))
if [ ${failed} -eq 0 ]; then
    echo -e "${BOLD}Summary:${RESET} ${GREEN}${succeeded}/${total} succeeded${RESET} in ${total_min}m${total_sec}s"
else
    echo -e "${BOLD}Summary:${RESET} ${GREEN}${succeeded}/${total} succeeded${RESET}, ${RED}${failed}/${total} failed${RESET} in ${total_min}m${total_sec}s"
    echo ""
    echo -e "${RED}Failed configurations:${RESET}"
    for config in "${failed_configs[@]}"; do
        echo "  - ${config}"
    done
    echo ""
    echo "To see detailed error for a specific configuration:"
    echo "  nix eval '${repo_root}#nixosConfigurations.<config>.config.system.build.toplevel.drvPath'"
    exit 1
fi
