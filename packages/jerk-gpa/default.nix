{
  writeShellApplication,
  systemd,
  coreutils,
  jq,
  niri,
  uwsm,
  protectedVars,
}:
let
  protectedArray = "(${builtins.concatStringsSep " " protectedVars})";
in
writeShellApplication {
  name = "jerk-gpa";
  runtimeInputs = [ systemd coreutils jq niri uwsm ];
  text = ''
    if [[ $# -ne 1 ]]; then
      echo >&2 "Usage: jerk-gpa <portal-url>"
      exit 1
    fi
    portal="$1"

    # Import vars from systemd user environment
    protected=${protectedArray}
    while IFS= read -r line; do
      [[ -z "$line" ]] && continue
      [[ "$line" != *=* ]] && continue
      var="''${line%%=*}"
      for p in "''${protected[@]}"; do
        if [[ "$var" == "$p" ]] || { [[ "$p" == *_ && ''${#p} -gt 1 ]] && [[ "$var" == "$p"* ]]; }; then
          continue 2
        fi
      done
      val="''${line#*=}"
      if declare -p "$var" &>/dev/null; then
        printf -v current_val '%s' "''${!var}"
      else
        current_val=""
      fi
      if [[ "$current_val" != "$val" ]]; then
        export "$var"="$val"
      fi
    done < <(systemctl show-environment --user 2>/dev/null)

    # Ensure Chrome is running (GPA uses it for the captive portal)
    has_chrome_window() {
      niri msg --json windows 2>/dev/null | jq -e 'any(.[]; .app_id == "google-chrome")' >/dev/null 2>&1
    }

    echo >&2 "Checking for Chrome window..."
    if has_chrome_window; then
      echo >&2 "Chrome is already running."
    else
      echo >&2 "Starting Chrome..."
      uwsm app -- google-chrome-stable &
      for ((i = 0; i < 10; i++)); do
        sleep 1
        if has_chrome_window; then
          echo >&2 "Chrome window appeared."
          break
        fi
      done
      if ! has_chrome_window; then
        echo >&2 "ERROR: Chrome window did not appear after 10 seconds"
        exit 1
      fi
    fi

    set -euo pipefail

    echo >&2 "Stopping GPA..."
    systemctl --user stop gpa.service
    echo >&2 "Cleaning GPA state..."
    rm -rf ~/.GlobalProtect
    echo >&2 "Starting GPA..."
    systemctl --user start gpa.service
    echo >&2 "Waiting for GPA to settle..."
    sleep 10
    echo >&2 "Connecting to $portal..."
    globalprotect connect --portal "$portal"
  '';
}
