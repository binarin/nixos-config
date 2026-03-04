#!/usr/bin/env bash
# Fetches open issues with label filtering
# Usage: fj-issues.sh [--exclude-label LABEL]... [--include-label LABEL]...
#
# Examples:
#   fj-issues.sh                           # All open issues
#   fj-issues.sh --exclude-label draft     # Exclude issues with 'draft' label
#   fj-issues.sh --include-label bug       # Only issues with 'bug' label
#
# Returns JSON array with: number, title, labels

set -euo pipefail

EXCLUDE_LABELS=()
INCLUDE_LABELS=()

while [[ $# -gt 0 ]]; do
    case $1 in
        --exclude-label)
            EXCLUDE_LABELS+=("$2")
            shift 2
            ;;
        --include-label)
            INCLUDE_LABELS+=("$2")
            shift 2
            ;;
        *)
            echo "Unknown option: $1" >&2
            echo "Usage: $0 [--exclude-label LABEL]... [--include-label LABEL]..." >&2
            exit 1
            ;;
    esac
done

KEYS_FILE="${HOME}/.local/share/forgejo-cli/keys.json"

if [[ ! -f "$KEYS_FILE" ]]; then
    echo "Error: keys.json not found at $KEYS_FILE" >&2
    exit 1
fi

# Get repo info from git remote
REMOTE_URL=$(git remote get-url origin 2>/dev/null || echo "")
if [[ -z "$REMOTE_URL" ]]; then
    echo "Error: Could not get git remote URL" >&2
    exit 1
fi

# Parse URL: https://forgejo.host/owner/repo or git@forgejo.host:owner/repo
if [[ "$REMOTE_URL" =~ ^https://([^/]+)/([^/]+)/([^/.]+) ]]; then
    HOST="${BASH_REMATCH[1]}"
    OWNER="${BASH_REMATCH[2]}"
    REPO="${BASH_REMATCH[3]}"
elif [[ "$REMOTE_URL" =~ ^git@([^:]+):([^/]+)/([^/.]+) ]]; then
    HOST="${BASH_REMATCH[1]}"
    OWNER="${BASH_REMATCH[2]}"
    REPO="${BASH_REMATCH[3]}"
else
    echo "Error: Could not parse remote URL: $REMOTE_URL" >&2
    exit 1
fi

# Extract token for host from keys.json
TOKEN=$(jq -r --arg host "$HOST" '.hosts[$host].token // empty' "$KEYS_FILE")
if [[ -z "$TOKEN" ]]; then
    echo "Error: No token found for host $HOST in $KEYS_FILE" >&2
    exit 1
fi

# Fetch open issues via API
API_URL="https://${HOST}/api/v1/repos/${OWNER}/${REPO}/issues?state=open&type=issues"

# Build jq filter for label filtering
JQ_FILTER='[.[] | {number: .number, title: .title, labels: [.labels[].name]}'

# Add include filter (must have ALL specified labels)
for label in "${INCLUDE_LABELS[@]}"; do
    JQ_FILTER+=" | select(.labels | index(\"$label\"))"
done

# Add exclude filter (must not have ANY specified labels)
for label in "${EXCLUDE_LABELS[@]}"; do
    JQ_FILTER+=" | select(.labels | index(\"$label\") | not)"
done

JQ_FILTER+=']'

curl -sf \
    -H "Authorization: token ${TOKEN}" \
    -H "Accept: application/json" \
    "$API_URL" | jq "$JQ_FILTER"
