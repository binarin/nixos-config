#!/usr/bin/env bash

set -euo pipefail

usage() {
    cat << EOF
Usage: $0 [OPTIONS] [HOSTNAME]

Options:
    -p, --principal PRINCIPAL   Principal name for certificate (default: binarin,root)
    -k, --key PUBLIC_KEY        Public key file to sign (default: search for id_ed25519.pub)
    -c, --ca-key CA_KEY         CA private key (default: /persist/$HOME/.ssh/keys.d/id_ed25519_sk_rk)
    -v, --validity PERIOD       Validity period (default: +1w)
    -h, --help                  Show this help message

Arguments:
    HOSTNAME                    Target hostname (default: localhost)

EOF
}

# Default values
principal="binarin,root"
ca_key="/persist$HOME/.ssh/keys.d/id_ed25519_sk_rk"
public_key=""
hostname="localhost"
validity="+1w"

# Parse options using getopt
TEMP=$(getopt -o p:k:c:v:h --long principal:,key:,ca-key:,validity:,help -n "$0" -- "$@")
if [ $? != 0 ]; then
    echo "Failed to parse options" >&2
    exit 1
fi

eval set -- "$TEMP"

while true; do
    case "$1" in
        -p|--principal)
            principal="$2"
            shift 2
            ;;
        -k|--key)
            public_key="$2"
            shift 2
            ;;
        -c|--ca-key)
            ca_key="$2"
            shift 2
            ;;
        -v|--validity)
            validity="$2"
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

# Get hostname from positional argument if provided
if [ $# -gt 0 ]; then
    hostname="$1"
fi

# Function to find public key
find_public_key() {
    local host="$1"

    if [ "$host" = "localhost" ]; then
        # Local search
        local search_dirs=("/persist$HOME/.ssh/keys.d" "$HOME/.ssh")

        for dir in "${search_dirs[@]}"; do
            if [ -f "$dir/id_ed25519.pub" ]; then
                echo "$dir/id_ed25519.pub"
                return 0
            fi
        done

        echo "Error: Could not find id_ed25519.pub in any of the local search directories: ${search_dirs[*]}" >&2
        exit 1
    else
        # Remote search - expand $HOME once on the remote host
        local remote_home=$(ssh "$host" "echo \$HOME")
        local remote_dirs=("/persist$remote_home/.ssh/keys.d" "$remote_home/.ssh")

        for dir in "${remote_dirs[@]}"; do
            if ssh "$host" "test -f '$dir/id_ed25519.pub'" 2>/dev/null; then
                echo "$dir/id_ed25519.pub"
                return 0
            fi
        done

        echo "Error: Could not find id_ed25519.pub in any of the remote search directories on $host: ${remote_dirs[*]}" >&2
        exit 1
    fi
}

# If no public key specified, search for it
if [ -z "$public_key" ]; then
    public_key=$(find_public_key "$hostname")
fi

# Validate that public key exists
if [ "$hostname" = "localhost" ]; then
    if [ ! -f "$public_key" ]; then
        echo "Error: Public key file '$public_key' does not exist" >&2
        exit 1
    fi
else
    if ! ssh "$hostname" "test -f '$public_key'" 2>/dev/null; then
        echo "Error: Public key file '$public_key' does not exist on $hostname" >&2
        exit 1
    fi
fi

# Validate that CA key exists
if [ ! -f "$ca_key" ]; then
    echo "Error: CA key file '$ca_key' does not exist" >&2
    exit 1
fi

# Handle localhost case vs remote host
if [ "$hostname" = "localhost" ]; then
    # For localhost, sign the key directly
    key_dir=$(dirname "$public_key")
    ssh-keygen -I "$hostname-$(date)" -s "$ca_key" -n "$principal" -V "$validity" "$public_key"
    echo "Certificate created: $key_dir/$(basename "$public_key" .pub)-cert.pub"
else
    # For remote hosts, use temporary directory and scp
    temp_dir=$(mktemp -d)
    trap "rm -rf '$temp_dir'" EXIT

    # Get the actual filename from the public key path
    pub_key_name=$(basename "$public_key")
    pub_key_dir=$(dirname "$public_key")
    cert_name="${pub_key_name%.pub}-cert.pub"

    ssh "$hostname" cat "$public_key" > "$temp_dir/$pub_key_name"

    ssh-keygen -I "$hostname-$(date)" -s "$ca_key" -n "$principal" -V "$validity" "$temp_dir/$pub_key_name"

    scp "$temp_dir/$cert_name" "$hostname:$pub_key_dir/"
fi
