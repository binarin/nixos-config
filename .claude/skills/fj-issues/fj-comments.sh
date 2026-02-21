#!/usr/bin/env bash
# Fetches issue comments with full metadata (id, timestamps, user)
# Usage: fj-comments.sh <issue_id>
#
# Returns JSON array with: id, body, created_at, updated_at, user.login
# This can be dropped when forgejo-cli adds structured output support.

set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <issue_id>" >&2
    exit 1
fi

ISSUE_ID="$1"
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

# Fetch comments via API
API_URL="https://${HOST}/api/v1/repos/${OWNER}/${REPO}/issues/${ISSUE_ID}/comments"

curl -sf \
    -H "Authorization: token ${TOKEN}" \
    -H "Accept: application/json" \
    "$API_URL" | jq '[.[] | {id, body, created_at, updated_at, user: .user.login}]'
