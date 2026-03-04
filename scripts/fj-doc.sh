#!/usr/bin/env bash
# Fetches docs/FJ.md from origin/master via HTTP API
# Usage: fj-doc.sh
#
# Outputs the content of docs/FJ.md to stdout.
# Uses the same token as fj-comments.sh for authentication.

set -euo pipefail

KEYS_FILE="${HOME}/.local/share/forgejo-cli/keys.json"

if [[ ! -f "$KEYS_FILE" ]]; then
    echo "Error: keys.json not found at $KEYS_FILE" >&2
    exit 1
fi

HOST="forgejo.lynx-lizard.ts.net"
OWNER="binarin"
REPO="nixos-config"
FILE_PATH="docs/FJ.md"
REF="master"

# Extract token for host from keys.json
TOKEN=$(jq -r --arg host "$HOST" '.hosts[$host].token // empty' "$KEYS_FILE")
if [[ -z "$TOKEN" ]]; then
    echo "Error: No token found for host $HOST in $KEYS_FILE" >&2
    exit 1
fi

# Fetch file content via API
API_URL="https://${HOST}/api/v1/repos/${OWNER}/${REPO}/raw/${FILE_PATH}?ref=${REF}"

curl -sf \
    -H "Authorization: token ${TOKEN}" \
    "$API_URL"
