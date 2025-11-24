#!/usr/bin/env bash
set -euo pipefail

# Get clipboard content
clipboard=$(wl-paste -p)

# Extract first line as title (trimmed)
title=$(echo "$clipboard" | head -n1 | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

# Extract rest as body and trim empty lines at start and end
body=$(echo "$clipboard" | tail -n +2 | awk 'NF {p=1} p' | tac | awk 'NF {p=1} p' | tac)

# URL encode function (RFC 3986)
urlencode() {
    local LC_ALL=C  # Process as bytes, not multi-byte characters
    local string="${1}"
    local strlen=${#string}
    local encoded=""
    local pos c o

    for (( pos=0 ; pos<strlen ; pos++ )); do
        c=${string:$pos:1}
        case "$c" in
            [-_.~a-zA-Z0-9] ) o="${c}" ;;
            * ) printf -v o '%%%02X' "'$c"
        esac
        encoded+="${o}"
    done
    echo "${encoded}"
}

# URL encode title and body
title_encoded=$(urlencode "$title")
body_encoded=$(urlencode "$body")

# Invoke emacsclient
emacsclient "org-protocol://capture?template=l&title=${title_encoded}&body=${body_encoded}"
