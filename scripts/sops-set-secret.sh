#!/usr/bin/env bash
set -euo pipefail

usage() {
    cat << 'EOF'
Usage: sops-set-secret.sh [OPTIONS] <sops-file> <key-path>

Generate a random password using apg and set it in a sops-encrypted YAML file.
The generated password is never printed to stdout/stderr for security.

Arguments:
    sops-file     Path to the sops-encrypted YAML file
    key-path      YAML path to the secret (e.g., 'service/password' or 'db.password')
                  Use '/' or '.' as path separator

Options:
    -l, --length N      Password length (default: 24)
    -M, --mode MODE     apg mode for character classes (default: SNCL)
                        N=Numeric, C=Capital, L=Lowercase, S=Special
    -h, --help          Show this help message

Examples:
    sops-set-secret.sh secrets/media/secrets.yaml jellyfin/api-key
    sops-set-secret.sh -l 32 secrets/forgejo/secrets.yaml runner-token
    sops-set-secret.sh -M NCL secrets/db/secrets.yaml postgres.password
EOF
}

# Default values
length=24
mode="SNCL"

# Parse options using getopt
TEMP=$(getopt -o l:M:h --long length:,mode:,help -n "$0" -- "$@")
if [ $? != 0 ]; then
    echo "Failed to parse options" >&2
    exit 1
fi

eval set -- "$TEMP"

while true; do
    case "$1" in
        -l|--length)
            length="$2"
            shift 2
            ;;
        -M|--mode)
            mode="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Internal error!" >&2
            exit 1
            ;;
    esac
done

# Check required arguments
if [ $# -lt 2 ]; then
    echo "Error: Missing required arguments" >&2
    usage >&2
    exit 1
fi

sops_file="$1"
key_path="$2"

# Validate sops file exists
if [ ! -f "$sops_file" ]; then
    echo "Error: Sops file '$sops_file' does not exist" >&2
    exit 1
fi

# Normalize key path: convert '/' and '.' separators to sops --set format
# sops --set expects: '["key1"]["key2"]' format
normalize_key_path() {
    local path="$1"
    # Replace both / and . with spaces, then wrap each part in ["..."]
    echo "$path" | tr '/.' '\n' | while read -r part; do
        [ -n "$part" ] && printf '["%s"]' "$part"
    done
}

sops_key_path=$(normalize_key_path "$key_path")

if [ -z "$sops_key_path" ]; then
    echo "Error: Invalid key path '$key_path'" >&2
    exit 1
fi

# Generate password using apg (output captured, never printed)
password=$(apg -M "$mode" -n 1 -m "$length" -x "$length")

# Set the secret using sops --set
# The password is passed as a JSON string value
sops --set "${sops_key_path} \"${password}\"" "$sops_file"

echo "Secret set at path: $key_path" >&2
