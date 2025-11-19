#!/usr/bin/env bash
set -euo pipefail

# Emacs launcher script that intelligently switches compositor workspaces
# before executing emacsclient commands.
#
# Usage: emacs-launcher.sh <elisp-command>
#
# This script:
# 1. Finds the workspace where Emacs is running
# 2. Switches to that workspace if not already there
# 3. Polls workspace status with exponential backoff (2ms→4ms→8ms→10ms, max 500ms)
# 4. Executes the elisp command via emacsclient
#
# The script defaults to using niri compositor commands, with hyprctl
# functions provided as reference/fallback.

# Maximum time to wait for workspace switch (milliseconds)
readonly MAX_WAIT_MS=500

# Polling intervals with exponential backoff (milliseconds)
readonly POLL_INTERVALS=(2 4 8 10)

#
# Niri compositor functions (default)
#

# Get the workspace ID where Emacs is running
# Prefers workspace named "emacs" if multiple Emacs frames exist
# Returns: workspace_id or empty string if not found
get_emacs_workspace_niri() {
    local windows workspaces
    windows=$(niri msg --json windows)
    workspaces=$(niri msg --json workspaces)

    # Get all workspace IDs where Emacs is running
    local emacs_workspace_ids
    emacs_workspace_ids=$(echo "$windows" | jq -r '.[] | select(.app_id == "emacs") | .workspace_id')

    # If no Emacs windows, return empty
    if [[ -z "$emacs_workspace_ids" ]]; then
        return
    fi

    # Check if any Emacs window is on workspace named "emacs"
    local preferred_id
    preferred_id=$(echo "$workspaces" | jq -r --argjson ids "[$(echo "$emacs_workspace_ids" | paste -sd,)]" \
        '.[] | select(.name == "emacs" and ([.id] | inside($ids))) | .id')

    if [[ -n "$preferred_id" ]]; then
        echo "$preferred_id"
    else
        # Fall back to first Emacs workspace
        echo "$emacs_workspace_ids" | head -n1
    fi
}

# Get the currently active workspace ID
# Returns: workspace_id or empty string
get_active_workspace_niri() {
    niri msg --json workspaces | jq -r '.[] | select(.is_active == true) | .id' | head -n1
}

# Switch to the specified workspace by ID
# Args: $1 - workspace_id
switch_workspace_niri() {
    local workspace_id="$1"
    niri msg action focus-workspace "$workspace_id" >/dev/null 2>&1
}

# Check if workspace switch is complete
# Args: $1 - target workspace_id
# Returns: 0 if active, 1 otherwise
is_workspace_active_niri() {
    local target_id="$1"
    local active_id
    active_id=$(get_active_workspace_niri)
    [[ "$active_id" == "$target_id" ]]
}

#
# Hyprland compositor functions (reference/fallback)
#

# Get the workspace ID where Emacs is running
# Prefers workspace named "emacs" if multiple Emacs frames exist
# Returns: workspace_id or empty string if not found
get_emacs_workspace_hyprctl() {
    local clients workspaces
    clients=$(hyprctl clients -j)
    workspaces=$(hyprctl workspaces -j)

    # Get all workspace IDs where Emacs is running
    local emacs_workspace_ids
    emacs_workspace_ids=$(echo "$clients" | jq -r '.[] | select(.class == "emacs" or .class == "Emacs") | .workspace.id')

    # If no Emacs windows, return empty
    if [[ -z "$emacs_workspace_ids" ]]; then
        return
    fi

    # Check if any Emacs window is on workspace named "emacs"
    local preferred_id
    preferred_id=$(echo "$workspaces" | jq -r --argjson ids "[$(echo "$emacs_workspace_ids" | paste -sd,)]" \
        '.[] | select(.name == "emacs" and ([.id] | inside($ids))) | .id')

    if [[ -n "$preferred_id" ]]; then
        echo "$preferred_id"
    else
        # Fall back to first Emacs workspace
        echo "$emacs_workspace_ids" | head -n1
    fi
}

# Get the currently active workspace ID
# Returns: workspace_id or empty string
get_active_workspace_hyprctl() {
    hyprctl activeworkspace -j | jq -r '.id'
}

# Switch to the specified workspace by ID
# Args: $1 - workspace_id
switch_workspace_hyprctl() {
    local workspace_id="$1"
    hyprctl dispatch workspace "$workspace_id" >/dev/null 2>&1
}

# Check if workspace switch is complete
# Args: $1 - target workspace_id
# Returns: 0 if active, 1 otherwise
is_workspace_active_hyprctl() {
    local target_id="$1"
    local active_id
    active_id=$(get_active_workspace_hyprctl)
    [[ "$active_id" == "$target_id" ]]
}

#
# Core logic
#

# Poll workspace status with exponential backoff
# Args: $1 - target workspace_id
# Returns: 0 if workspace became active within timeout, 1 otherwise
wait_for_workspace_switch() {
    local target_id="$1"
    local elapsed_ms=0
    local interval_idx=0
    local max_interval_idx=$((${#POLL_INTERVALS[@]} - 1))

    while (( elapsed_ms < MAX_WAIT_MS )); do
        if is_workspace_active_niri "$target_id"; then
            return 0
        fi

        # Get current interval (use last value if we've exceeded array)
        local interval
        if (( interval_idx <= max_interval_idx )); then
            interval=${POLL_INTERVALS[$interval_idx]}
            ((interval_idx++))
        else
            interval=${POLL_INTERVALS[$max_interval_idx]}
        fi

        # Sleep for interval (convert ms to seconds for sleep)
        sleep "$(printf "0.%03d" "$interval")"
        ((elapsed_ms += interval))
    done

    return 1
}

# Main execution
main() {
    if [[ $# -eq 0 ]]; then
        echo "Usage: $0 <elisp-command>" >&2
        echo "Example: $0 '(universal-launcher-popup)'" >&2
        exit 1
    fi

    # Get Emacs workspace
    local emacs_workspace
    emacs_workspace=$(get_emacs_workspace_niri)

    # If Emacs is not running, just execute the command
    if [[ -z "$emacs_workspace" ]]; then
        exec emacsclient --eval "$@"
    fi

    # Get current workspace
    local current_workspace
    current_workspace=$(get_active_workspace_niri)

    # If we're already on the Emacs workspace, execute immediately
    if [[ "$current_workspace" == "$emacs_workspace" ]]; then
        exec emacsclient --eval "$@"
    fi

    # Switch to Emacs workspace
    switch_workspace_niri "$emacs_workspace"

    # Wait for workspace switch to complete (with timeout)
    # Note: We execute the command regardless of whether the switch completes,
    # matching the behavior of the original Go implementation
    wait_for_workspace_switch "$emacs_workspace" || true

    # Execute the emacsclient command
    exec emacsclient --eval "$@"
}

main "$@"
