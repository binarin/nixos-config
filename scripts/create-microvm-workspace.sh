#!/usr/bin/env bash
# Script to create a new microVM workspace directory
# Usage: create-microvm-workspace.sh <vm-name>

set -euo pipefail

VM_NAME="${1:-}"
WORKSPACE_BASE="/persist/home/binarin/workspaces"

if [[ -z "$VM_NAME" ]]; then
    echo "Usage: $0 <vm-name>"
    exit 1
fi

WORKSPACE_DIR="$WORKSPACE_BASE/$VM_NAME"

if [[ -d "$WORKSPACE_DIR" ]]; then
    echo "Workspace directory already exists: $WORKSPACE_DIR"
    exit 1
fi

echo "Creating workspace directory: $WORKSPACE_DIR"
mkdir -p "$WORKSPACE_DIR"

# Create SSH host keys directory
SSH_KEYS_DIR="$WORKSPACE_DIR/ssh-host-keys"
mkdir -p "$SSH_KEYS_DIR"

# Generate SSH host keys
echo "Generating SSH host keys..."
ssh-keygen -t ed25519 -f "$SSH_KEYS_DIR/ssh_host_ed25519_key" -N "" -C "$VM_NAME"
ssh-keygen -t rsa -b 4096 -f "$SSH_KEYS_DIR/ssh_host_rsa_key" -N "" -C "$VM_NAME"

# Copy user's authorized_keys if it exists
if [[ -f "$HOME/.ssh/authorized_keys" ]]; then
    echo "Copying authorized_keys..."
    cp "$HOME/.ssh/authorized_keys" "$SSH_KEYS_DIR/authorized_keys"
elif [[ -f "$HOME/.ssh/id_ed25519.pub" ]]; then
    echo "Creating authorized_keys from id_ed25519.pub..."
    cp "$HOME/.ssh/id_ed25519.pub" "$SSH_KEYS_DIR/authorized_keys"
elif [[ -f "$HOME/.ssh/id_rsa.pub" ]]; then
    echo "Creating authorized_keys from id_rsa.pub..."
    cp "$HOME/.ssh/id_rsa.pub" "$SSH_KEYS_DIR/authorized_keys"
else
    echo "Warning: No SSH public keys found. You'll need to create $SSH_KEYS_DIR/authorized_keys manually."
    touch "$SSH_KEYS_DIR/authorized_keys"
fi

# Set proper ownership
chown -R "$(id -u):$(id -g)" "$WORKSPACE_DIR"
chmod 700 "$WORKSPACE_DIR"
chmod 700 "$SSH_KEYS_DIR"
chmod 600 "$SSH_KEYS_DIR"/*

echo "Workspace created successfully at: $WORKSPACE_DIR"
echo ""
echo "Next steps:"
echo "1. Add the VM configuration to modules/microvm-vms.nix"
echo "2. Rebuild the system configuration"
echo "3. Start the VM with: sudo systemctl start microvm@$VM_NAME"
echo "4. SSH to the VM with: ssh <ip-address>"
