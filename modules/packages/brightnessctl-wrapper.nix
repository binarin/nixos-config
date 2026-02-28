{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.brightnessctl-wrapper = pkgs.writeShellApplication {
        name = "brightnessctl-wrapper";
        runtimeInputs = with pkgs; [
          ddcutil
          jq
          coreutils
          gawk
          brightnessctl
          niri
          getopt
          gnused
        ];
        text = ''
          # brightnessctl wrapper that handles DDC monitors via ddcutil
          # Supports: -s s PERCENT (save and set), -r (restore)

          STATE_DIR="''${XDG_RUNTIME_DIR:-/tmp}/brightness-wrapper"
          mkdir -p "$STATE_DIR"

          # Parse arguments using getopt
          SAVE_MODE=false
          RESTORE_MODE=false
          SET_VALUE=""

          PARSED=$(getopt -o sr -n 'brightnessctl-wrapper' -- "$@") || exit 1
          eval set -- "$PARSED"

          while true; do
            case "$1" in
              -s)
                SAVE_MODE=true
                shift
                ;;
              -r)
                RESTORE_MODE=true
                shift
                ;;
              --)
                shift
                break
                ;;
              *)
                echo "Unexpected option: $1" >&2
                exit 1
                ;;
            esac
          done

          # Remaining positional arguments: command and value (e.g., "s 10%")
          if [[ $# -ge 2 && "$1" == "s" ]]; then
            SET_VALUE="$2"
          fi

          # Get active monitors from niri (those with current_mode != null)
          get_active_outputs() {
            niri msg -j outputs | jq -r '.[] | select(.current_mode != null) | .name'
          }

          # Get DDC monitor info: maps connector to i2c bus
          # Returns lines like: DP-1 15
          get_ddc_monitors() {
            ddcutil detect --brief 2>/dev/null | awk '
              /I2C bus:/ { bus = $NF; sub(/.*-/, "", bus) }
              /DRM connector:/ { connector = $NF; sub(/card[0-9]+-/, "", connector) }
              /Monitor:/ { if (bus && connector) print connector, bus; bus=""; connector="" }
            '
          }

          # Check if a monitor is a DDC monitor (external, like DELL U4025QW)
          # For now, we consider any monitor found by ddcutil as DDC-capable
          is_ddc_monitor() {
            local connector="$1"
            get_ddc_monitors | grep -q "^$connector "
          }

          # Get i2c bus for a connector
          get_bus_for_connector() {
            local connector="$1"
            get_ddc_monitors | awk -v conn="$connector" '$1 == conn { print $2 }'
          }

          # Get current brightness from DDC monitor
          get_ddc_brightness() {
            local bus="$1"
            ddcutil -b "$bus" getvcp 10 2>/dev/null | grep -oP 'current value =\s*\K\d+' || true
          }

          # Set DDC brightness
          set_ddc_brightness() {
            local bus="$1"
            local value="$2"
            ddcutil -b "$bus" setvcp 10 "$value" 2>/dev/null
          }

          # Convert percentage string (e.g., "10%") to integer
          percent_to_int() {
            local val="$1"
            val="''${val%\%}"
            echo "$val"
          }

          # Handle save and set for DDC monitors
          # Args: bus percent save_mode state_dir
          handle_ddc_save_set() {
            local bus="$1"
            local percent="$2"
            local save_mode="$3"
            local state_dir="$4"
            local value
            value=$(percent_to_int "$percent")

            if [[ "$save_mode" == "true" ]]; then
              local current
              current=$(get_ddc_brightness "$bus")
              if [[ -n "$current" ]]; then
                echo "$current" > "$state_dir/ddc-$bus.brightness"
              fi
            fi

            set_ddc_brightness "$bus" "$value"
          }

          # Handle restore for DDC monitors
          # Args: bus state_dir
          handle_ddc_restore() {
            local bus="$1"
            local state_dir="$2"
            local saved_file="$state_dir/ddc-$bus.brightness"

            if [[ -f "$saved_file" ]]; then
              local saved_value
              saved_value=$(cat "$saved_file")
              set_ddc_brightness "$bus" "$saved_value"
              rm -f "$saved_file"
            fi
          }

          # Main logic
          pids=()

          if [[ "$RESTORE_MODE" == "true" ]]; then
            # Restore mode: restore all saved DDC monitors, and call brightnessctl -r for laptop

            # Restore DDC monitors
            for saved_file in "$STATE_DIR"/ddc-*.brightness; do
              if [[ -f "$saved_file" ]]; then
                bus=$(basename "$saved_file" | sed 's/ddc-//;s/.brightness//')
                handle_ddc_restore "$bus" "$STATE_DIR" &
                pids+=($!)
              fi
            done

            # Restore laptop display
            brightnessctl -r &
            pids+=($!)

          elif [[ -n "$SET_VALUE" ]]; then
            # Set mode: set brightness on all active monitors

            mapfile -t active_outputs < <(get_active_outputs)

            for output in "''${active_outputs[@]}"; do
              bus=$(get_bus_for_connector "$output")
              if [[ -n "$bus" ]]; then
                # DDC monitor
                handle_ddc_save_set "$bus" "$SET_VALUE" "$SAVE_MODE" "$STATE_DIR" &
                pids+=($!)
              else
                # Laptop display - use brightnessctl
                if [[ "$SAVE_MODE" == "true" ]]; then
                  brightnessctl -s s "$SET_VALUE" &
                else
                  brightnessctl s "$SET_VALUE" &
                fi
                pids+=($!)
              fi
            done
          fi

          # Wait for all background processes
          for pid in "''${pids[@]}"; do
            wait "$pid" 2>/dev/null || true
          done
        '';
      };
    };
}
