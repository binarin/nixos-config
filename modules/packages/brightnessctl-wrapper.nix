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
          gnugrep
        ];
        text = ''
          # brightnessctl wrapper that handles DDC monitors via ddcutil
          # Supports: -s s PERCENT (save and set), -r (restore)

          STATE_DIR="''${XDG_RUNTIME_DIR:-/tmp}/brightness-wrapper"
          CACHE_DIR="$STATE_DIR/cache"
          mkdir -p "$STATE_DIR" "$CACHE_DIR"

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

          # Run full ddcutil detect and parse output
          # Returns lines like: DP-1 15
          run_ddc_detect() {
            ddcutil detect --brief 2>/dev/null | awk '
              /I2C bus:/ { bus = $NF; sub(/.*-/, "", bus) }
              /DRM connector:/ { connector = $NF; sub(/card[0-9]+-/, "", connector) }
              /Monitor:/ { if (bus && connector) print connector, bus; bus=""; connector="" }
            '
          }

          # Verify that a cached bus still corresponds to the expected connector
          # Uses ddcutil detect on specific bus to check connector name
          verify_bus_connector() {
            local bus="$1"
            local expected_connector="$2"
            local actual_connector
            actual_connector=$(ddcutil detect --brief --bus "$bus" 2>/dev/null | awk '
              /DRM connector:/ { connector = $NF; sub(/card[0-9]+-/, "", connector); print connector }
            ')
            [[ "$actual_connector" == "$expected_connector" ]]
          }

          # Get cached bus for a connector, or return empty if cache miss/invalid
          get_cached_bus() {
            local connector="$1"
            local cache_file="$CACHE_DIR/$connector.bus"
            if [[ -f "$cache_file" ]]; then
              local cached_bus
              cached_bus=$(cat "$cache_file")
              if verify_bus_connector "$cached_bus" "$connector"; then
                echo "$cached_bus"
                return 0
              fi
              # Cache invalid, remove it
              rm -f "$cache_file"
            fi
            return 1
          }

          # Save bus to cache for a connector
          cache_bus() {
            local connector="$1"
            local bus="$2"
            echo "$bus" > "$CACHE_DIR/$connector.bus"
          }

          # Get i2c bus for a connector (uses cache, falls back to full detect)
          get_bus_for_connector() {
            local connector="$1"
            local bus

            # Try cache first
            if bus=$(get_cached_bus "$connector"); then
              echo "$bus"
              return 0
            fi

            # Cache miss - run full detect
            local detect_output
            detect_output=$(run_ddc_detect)

            # Update cache for all detected monitors
            while read -r conn b; do
              cache_bus "$conn" "$b"
            done <<< "$detect_output"

            # Return bus for requested connector
            bus=$(echo "$detect_output" | awk -v conn="$connector" '$1 == conn { print $2 }')
            if [[ -n "$bus" ]]; then
              echo "$bus"
              return 0
            fi
            return 1
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
                local state_file="$state_dir/ddc-$bus.brightness"
                if [[ -f "$state_file" ]]; then
                  local old_saved
                  old_saved=$(cat "$state_file")
                  echo "Saving DDC bus $bus brightness: $current (overwriting previous saved value $old_saved)"
                else
                  echo "Saving DDC bus $bus brightness: $current"
                fi
                echo "$current" > "$state_file"
              fi
            fi

            echo "Setting DDC bus $bus brightness to $value (via ddcutil)"
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
              echo "Setting DDC bus $bus brightness to $saved_value (via ddcutil)"
              set_ddc_brightness "$bus" "$saved_value"
              rm -f "$saved_file"
            fi
          }

          # Main logic
          pids=()

          if [[ "$RESTORE_MODE" == "true" ]]; then
            # Restore mode: restore saved monitors

            for saved_file in "$STATE_DIR"/ddc-*.brightness; do
              if [[ -f "$saved_file" ]]; then
                bus=$(basename "$saved_file" | sed 's/ddc-//;s/.brightness//')
                saved_value=$(cat "$saved_file")
                echo "Restoring DDC bus $bus brightness to $saved_value"
                handle_ddc_restore "$bus" "$STATE_DIR" &
                pids+=($!)
              fi
            done

            # Check if laptop brightness was saved (brightnessctl saves to its own location)
            if [[ -f "$STATE_DIR/has-laptop" ]]; then
              rm -f "$STATE_DIR/has-laptop"
              echo "Restoring laptop display brightness via brightnessctl"
              brightnessctl -r &
              pids+=($!)
            fi

          elif [[ -n "$SET_VALUE" ]]; then
            # Set mode: set brightness on all active monitors

            mapfile -t active_outputs < <(get_active_outputs)

            has_laptop=false
            ddc_buses=()

            # First pass: identify monitor types
            for output in "''${active_outputs[@]}"; do
              if bus=$(get_bus_for_connector "$output"); then
                ddc_buses+=("$bus")
              else
                has_laptop=true
              fi
            done

            # Handle DDC monitors
            for bus in "''${ddc_buses[@]}"; do
              handle_ddc_save_set "$bus" "$SET_VALUE" "$SAVE_MODE" "$STATE_DIR" &
              pids+=($!)
            done

            # Handle laptop display only if there are non-DDC monitors
            if [[ "$has_laptop" == "true" ]]; then
              # Mark that we have laptop brightness to restore
              if [[ "$SAVE_MODE" == "true" ]]; then
                echo "Saving and setting laptop display brightness to $SET_VALUE (via brightnessctl)"
                touch "$STATE_DIR/has-laptop"
                brightnessctl -s s "$SET_VALUE" &
              else
                echo "Setting laptop display brightness to $SET_VALUE (via brightnessctl)"
                brightnessctl s "$SET_VALUE" &
              fi
              pids+=($!)
            fi
          fi

          # Wait for all background processes
          for pid in "''${pids[@]}"; do
            wait "$pid" 2>/dev/null || true
          done
        '';
      };
    };
}
