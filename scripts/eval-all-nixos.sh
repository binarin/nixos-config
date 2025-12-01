#!/usr/bin/env bash
set -euo pipefail

# Disable output buffering
exec 2>&1

# Evaluate all NixOS configurations in parallel and report success/failure
# Usage: eval-all-nixos.sh [nix-options...]
#
# Primary use case: Debugging infinite recursion and evaluation errors.
# When `nix flake check` fails, it evaluates all outputs together, making
# it difficult to identify which specific configuration is causing the problem.
# This script evaluates each configuration individually in parallel, isolating
# failures and providing clear diagnostics about which configs have issues.

# Handle Ctrl-C and other signals
cleanup() {
    echo -e "\n\nInterrupted by user"
    # Kill all background jobs
    jobs -p | xargs -r kill 2>/dev/null || true
    # Clean up temp directory
    [ -n "${tmpdir:-}" ] && rm -rf "${tmpdir}"
    exit 130
}
trap cleanup INT TERM

# ANSI colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
RESET='\033[0m'

repo_root="$(pwd)"
nix_options=("$@")

# Determine parallelism (default to number of CPUs)
max_jobs=${NIX_BUILD_CORES:-$(nproc)}

# Create temporary directory for job management
tmpdir=$(mktemp -d)
trap 'rm -rf "${tmpdir}"' EXIT

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

echo "Found ${total} configurations to evaluate (parallelism: ${max_jobs})"
echo ""

# Initialize counters
echo "0" > "${tmpdir}/completed"
echo "0" > "${tmpdir}/succeeded"
echo "0" > "${tmpdir}/failed"

# Function to evaluate a single configuration
eval_config() {
    local config=$1
    local index=$2
    local output_file="${tmpdir}/${index}.output"
    local status_file="${tmpdir}/${index}.status"
    local time_file="${tmpdir}/${index}.time"

    local start_time=$(date +%s)

    # Try to evaluate the configuration
    if nix eval --raw "${repo_root}#nixosConfigurations.${config}.config.system.build.toplevel.drvPath" "${nix_options[@]}" >&"${output_file}" 2>&1; then
        echo "OK" > "${status_file}"
    else
        echo "FAILED" > "${status_file}"
    fi

    local end_time=$(date +%s)
    echo "$((end_time - start_time))" > "${time_file}"

    # Update counters atomically using flock
    (
        flock -x 200
        completed=$(cat "${tmpdir}/completed")
        completed=$((completed + 1))
        echo "${completed}" > "${tmpdir}/completed"

        if [ "$(cat "${status_file}")" = "OK" ]; then
            succeeded=$(cat "${tmpdir}/succeeded")
            succeeded=$((succeeded + 1))
            echo "${succeeded}" > "${tmpdir}/succeeded"
        else
            failed=$(cat "${tmpdir}/failed")
            failed=$((failed + 1))
            echo "${failed}" > "${tmpdir}/failed"
        fi
    ) 200>"${tmpdir}/lock"
}

# Start all evaluations in parallel
start_time=$(date +%s)
pids=()

for i in "${!configs[@]}"; do
    config="${configs[$i]}"

    # Start evaluation in background
    eval_config "${config}" "${i}" &
    pids[$i]=$!

    # Limit parallelism
    while [ $(jobs -r | wc -l) -ge ${max_jobs} ]; do
        sleep 0.1
    done
done

# Progress monitoring
last_completed=0
first_update=true
while [ $(jobs -r | wc -l) -gt 0 ]; do
    completed=$(cat "${tmpdir}/completed" 2>/dev/null || echo "0")
    succeeded=$(cat "${tmpdir}/succeeded" 2>/dev/null || echo "0")
    failed=$(cat "${tmpdir}/failed" 2>/dev/null || echo "0")

    # Only update display if progress changed
    if [ ${completed} -ne ${last_completed} ]; then
        if [ "${first_update}" = "true" ]; then
            # First update - just write the line
            echo -ne "${BLUE}Progress:${RESET} ${completed}/${total} "
            first_update=false
        else
            # Subsequent updates - clear and rewrite
            echo -ne "\r${BLUE}Progress:${RESET} ${completed}/${total} "
        fi
        echo -ne "(${GREEN}✓${succeeded}${RESET}"
        if [ ${failed} -gt 0 ]; then
            echo -ne " ${RED}✗${failed}${RESET}"
        fi
        echo -ne ")    "
        last_completed=${completed}
    fi

    sleep 0.2
done

# Wait for all jobs to complete
for pid in "${pids[@]}"; do
    wait "${pid}" 2>/dev/null || true
done

echo "" # New line after progress

end_time=$(date +%s)
total_time=$((end_time - start_time))

# Display results in order
echo ""
echo "Results:"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

succeeded=0
failed=0
declare -a failed_configs

for i in "${!configs[@]}"; do
    config="${configs[$i]}"
    status=$(cat "${tmpdir}/${i}.status" 2>/dev/null || echo "UNKNOWN")
    eval_time=$(cat "${tmpdir}/${i}.time" 2>/dev/null || echo "0")

    printf "[%2d/%2d] %-30s " "$((i + 1))" "${total}" "${config}:"

    if [ "${status}" = "OK" ]; then
        printf "${GREEN}✓ %-6s${RESET} (%3ds)\n" "OK" "${eval_time}"
        succeeded=$((succeeded + 1))
    else
        printf "${RED}✗ %-6s${RESET} (%3ds)\n" "FAILED" "${eval_time}"
        failed=$((failed + 1))
        failed_configs+=("${config}")
    fi
done

# Summary
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
total_min=$((total_time / 60))
total_sec=$((total_time % 60))

if [ ${failed} -eq 0 ]; then
    echo -e "${BOLD}Summary:${RESET} ${GREEN}${succeeded}/${total} succeeded${RESET} in ${total_min}m${total_sec}s (parallel)"
else
    echo -e "${BOLD}Summary:${RESET} ${GREEN}${succeeded}/${total} succeeded${RESET}, ${RED}${failed}/${total} failed${RESET} in ${total_min}m${total_sec}s (parallel)"
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
