# modules/packages/proxmox-vm-create.nix
{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.proxmox-vm-create = pkgs.writeShellApplication {
        name = "proxmox-vm-create";
        runtimeInputs = with pkgs; [
          jq
          nix
        ];
        text = ''
          set -euo pipefail

          usage() {
            echo "Usage: proxmox-vm-create <configuration-name> [--vmid <id>] [--dry-run]"
            echo ""
            echo "Generates Proxmox qm commands to create a VM from NixOS configuration."
            echo ""
            echo "Options:"
            echo "  --vmid <id>   Override VM ID from configuration (required if not set in config)"
            echo "  --dry-run     Print commands without executing"
            echo ""
            echo "Examples:"
            echo "  proxmox-vm-create claude-nixos-config --vmid 200 --dry-run"
            echo "  ssh root@valak \"\$(proxmox-vm-create claude-nixos-config --vmid 200 --dry-run)\""
            exit 1
          }

          if [ $# -lt 1 ]; then
            usage
          fi

          CONFIG_NAME="$1"
          shift

          VMID=""
          DRY_RUN=false

          while [ $# -gt 0 ]; do
            case "$1" in
              --vmid)
                VMID="$2"
                shift 2
                ;;
              --dry-run)
                DRY_RUN=true
                shift
                ;;
              -h|--help)
                usage
                ;;
              *)
                echo "Unknown option: $1"
                usage
                ;;
            esac
          done

          # Extract proxmox metadata from configuration
          METADATA=$(nix eval ".#nixosConfigurations.$CONFIG_NAME.config.nixos-config.qemu-guest.proxmox" --json 2>/dev/null) || {
            echo "ERROR: Could not evaluate configuration '$CONFIG_NAME'"
            echo "Make sure the configuration exists and has nixos-config.qemu-guest.proxmox options set."
            exit 1
          }

          if [ -z "$VMID" ]; then
            VMID=$(echo "$METADATA" | jq -r '.vmId // empty')
            if [ -z "$VMID" ]; then
              echo "ERROR: VM ID not specified and not set in configuration"
              echo "Use --vmid <id> or set nixos-config.qemu-guest.proxmox.vmId"
              exit 1
            fi
          fi

          HOSTNAME=$(nix eval ".#nixosConfigurations.$CONFIG_NAME.config.networking.hostName" --raw 2>/dev/null)
          MEMORY=$(echo "$METADATA" | jq -r '.memory')
          CORES=$(echo "$METADATA" | jq -r '.cores')
          SOCKETS=$(echo "$METADATA" | jq -r '.sockets // 1')
          BIOS=$(echo "$METADATA" | jq -r '.bios // "seabios"')
          MACHINE=$(echo "$METADATA" | jq -r '.machine // "q35"')
          SCSIHW=$(echo "$METADATA" | jq -r '.scsihw // "virtio-scsi-single"')
          ONBOOT=$(echo "$METADATA" | jq -r '.onboot // false')
          AGENT=$(echo "$METADATA" | jq -r '.agent // true')
          DESCRIPTION=$(echo "$METADATA" | jq -r '.description // empty')

          # Network config
          BRIDGE=$(echo "$METADATA" | jq -r '.network.bridge // "vmbr0"')
          NET_MODEL=$(echo "$METADATA" | jq -r '.network.model // "virtio"')
          FIREWALL=$(echo "$METADATA" | jq -r '.network.firewall // true')

          # Build commands array
          CMDS=()

          # Basic VM creation command
          CREATE_CMD="qm create $VMID --name $HOSTNAME --memory $MEMORY --cores $CORES --sockets $SOCKETS"
          CREATE_CMD+=" --bios $BIOS --machine $MACHINE --scsihw $SCSIHW"

          if [ "$ONBOOT" = "true" ]; then
            CREATE_CMD+=" --onboot 1"
          fi

          if [ "$AGENT" = "true" ]; then
            CREATE_CMD+=" --agent 1"
          fi

          if [ -n "$DESCRIPTION" ]; then
            CREATE_CMD+=" --description '$DESCRIPTION'"
          fi

          # Network
          FIREWALL_FLAG=0
          if [ "$FIREWALL" = "true" ]; then
            FIREWALL_FLAG=1
          fi
          CREATE_CMD+=" --net0 $NET_MODEL,bridge=$BRIDGE,firewall=$FIREWALL_FLAG"

          # Serial console for NixOS (always add for NixOS VMs)
          CREATE_CMD+=" --serial0 socket --vga serial0"

          CMDS+=("$CREATE_CMD")

          # Process disks
          DISK_COUNT=$(echo "$METADATA" | jq '.disks | length')
          BOOT_DISKS=()

          for i in $(seq 0 $((DISK_COUNT - 1))); do
            DISK=$(echo "$METADATA" | jq ".disks[$i]")
            DISK_TYPE=$(echo "$DISK" | jq -r '.type')
            BUS=$(echo "$DISK" | jq -r '.bus // "scsi"')
            INDEX=$(echo "$DISK" | jq -r '.index // 0')
            BOOT_ORDER=$(echo "$DISK" | jq -r '.bootOrder // empty')

            DISK_KEY="$BUS$INDEX"

            case "$DISK_TYPE" in
              passthrough)
                DEVICE=$(echo "$DISK" | jq -r '.device')
                CMDS+=("qm set $VMID --$DISK_KEY $DEVICE")
                ;;
              image)
                STORAGE=$(echo "$DISK" | jq -r '.storage')
                SIZE=$(echo "$DISK" | jq -r '.size')
                # For image type, size is specified as storage:size (e.g., local-zfs:32)
                SIZE_NUM="''${SIZE//[!0-9]/}"
                CMDS+=("qm set $VMID --$DISK_KEY $STORAGE:$SIZE_NUM")
                ;;
            esac

            if [ -n "$BOOT_ORDER" ]; then
              BOOT_DISKS+=("$BOOT_ORDER:$DISK_KEY")
            fi
          done

          # Set boot order if any disks have bootOrder specified
          if [ ''${#BOOT_DISKS[@]} -gt 0 ]; then
            # Sort by boot order number and join disk keys
            BOOT_ORDER_STR=$(printf '%s\n' "''${BOOT_DISKS[@]}" | sort -t: -k1 -n | cut -d: -f2 | tr '\n' ';' | sed 's/;$//')
            CMDS+=("qm set $VMID --boot order=$BOOT_ORDER_STR")
          fi

          # Output or execute commands
          if [ "$DRY_RUN" = "true" ]; then
            for cmd in "''${CMDS[@]}"; do
              echo "$cmd"
            done
          else
            for cmd in "''${CMDS[@]}"; do
              echo "Executing: $cmd"
              eval "$cmd"
            done
          fi
        '';
      };
    };
}
