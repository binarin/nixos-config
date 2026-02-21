#!/usr/bin/env bash
#
# Inject WiFi credentials into NixOS ISO image
#
# This script creates a NetworkManager connection file and injects it into
# the ISO image's squashfs. This allows WiFi connectivity without embedding
# credentials in the nix store during build time.
#
# Usage: inject-iso-wifi.sh <iso-file> <ssid> <password> [output-iso]
#
# Example:
#   ./scripts/inject-iso-wifi.sh result/iso/nixos.iso "agares-guest" "mypassword"
#

set -euo pipefail

if [[ $# -lt 3 ]]; then
    echo "Usage: $0 <iso-file> <ssid> <password> [output-iso]"
    echo ""
    echo "Arguments:"
    echo "  iso-file   - Path to the NixOS ISO image"
    echo "  ssid       - WiFi network name"
    echo "  password   - WiFi password (WPA-PSK)"
    echo "  output-iso - Output ISO path (default: adds -wifi suffix)"
    exit 1
fi

ISO_FILE="$1"
SSID="$2"
PASSWORD="$3"
OUTPUT_ISO="${4:-${ISO_FILE%.iso}-wifi.iso}"

if [[ ! -f "$ISO_FILE" ]]; then
    echo "Error: ISO file not found: $ISO_FILE"
    exit 1
fi

# Create NetworkManager connection file
NM_CONNECTION="[connection]
id=${SSID}
type=wifi

[wifi]
mode=infrastructure
ssid=${SSID}

[wifi-security]
key-mgmt=wpa-psk
psk=${PASSWORD}

[ipv4]
method=auto

[ipv6]
addr-gen-mode=stable-privacy
method=auto
"

WORK_DIR=$(mktemp -d)
trap 'rm -rf "$WORK_DIR"' EXIT

echo "Creating NetworkManager connection file..."
mkdir -p "$WORK_DIR/etc/NetworkManager/system-connections"
echo "$NM_CONNECTION" > "$WORK_DIR/etc/NetworkManager/system-connections/${SSID}.nmconnection"
chmod 600 "$WORK_DIR/etc/NetworkManager/system-connections/${SSID}.nmconnection"

echo "Extracting ISO..."
ISO_EXTRACT="$WORK_DIR/iso"
mkdir -p "$ISO_EXTRACT"

# Mount ISO and copy contents
ISO_MOUNT="$WORK_DIR/iso-mount"
mkdir -p "$ISO_MOUNT"
sudo mount -o loop "$ISO_FILE" "$ISO_MOUNT"
cp -a "$ISO_MOUNT"/* "$ISO_EXTRACT/"
sudo umount "$ISO_MOUNT"

# Find and extract squashfs
SQUASHFS=$(find "$ISO_EXTRACT" -name "*.squashfs" | head -1)
if [[ -z "$SQUASHFS" ]]; then
    echo "Error: No squashfs found in ISO"
    exit 1
fi

echo "Extracting squashfs..."
SQUASH_EXTRACT="$WORK_DIR/squashfs"
sudo unsquashfs -d "$SQUASH_EXTRACT" "$SQUASHFS"

# Inject NetworkManager connection
echo "Injecting WiFi credentials..."
sudo mkdir -p "$SQUASH_EXTRACT/etc/NetworkManager/system-connections"
sudo cp "$WORK_DIR/etc/NetworkManager/system-connections/${SSID}.nmconnection" \
    "$SQUASH_EXTRACT/etc/NetworkManager/system-connections/"
sudo chmod 600 "$SQUASH_EXTRACT/etc/NetworkManager/system-connections/${SSID}.nmconnection"

# Repack squashfs
echo "Repacking squashfs..."
sudo rm "$SQUASHFS"
sudo mksquashfs "$SQUASH_EXTRACT" "$SQUASHFS" -comp gzip -Xcompression-level 1

# Recreate ISO
echo "Creating new ISO..."
xorriso -as mkisofs \
    -o "$OUTPUT_ISO" \
    -R -J -joliet-long \
    -b boot/grub/i386-pc/eltorito.img \
    -no-emul-boot -boot-load-size 4 -boot-info-table \
    --efi-boot boot/efi.img \
    -efi-boot-part --efi-boot-image \
    "$ISO_EXTRACT"

echo "Done! WiFi-enabled ISO created at: $OUTPUT_ISO"
