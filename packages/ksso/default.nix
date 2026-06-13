{
  writeShellApplication,
  systemd,
  coreutils,
  gnugrep,
  niri,
  jq,
  uwsm,
  openssh,
  xdg-utils,
  protectedVars,
}:
let
  protectedArray = "(${builtins.concatStringsSep " " protectedVars})";
in
writeShellApplication {
  name = "ksso";
  runtimeInputs = [ systemd coreutils gnugrep niri jq uwsm openssh xdg-utils ];
  text = ''
    if [[ $# -ne 3 ]]; then
      echo >&2 "Usage: ksso <ssh-host> <kick-off-sso-command> <on-successful-auth-command>"
      exit 1
    fi
    ssh_host="$1"
    kick_off_sso_command="$2"
    on_successful_auth_command="$3"

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
      if [[ -v "$var" ]]; then
        printf -v current_val '%s' "''${!var}"
        if [[ "$current_val" != "$val" ]]; then
          export "$var"="$val"
        fi
      else
        export "$var"="$val"
      fi
    done < <(systemctl show-environment --user 2>/dev/null)

    set -euo pipefail

    trap 'echo >&2 "✗ Interrupted"; exit 130' INT

    # Pre-start Google Chrome via uwsm so it outlives this script
    has_chrome_window() {
      niri msg --json windows 2>/dev/null \
        | jq -e 'any(.[]; .app_id == "google-chrome")' >/dev/null 2>&1
    }
    if ! has_chrome_window; then
      echo >&2 "→ Pre-starting Google Chrome..."
      uwsm app -t service -- google-chrome-stable &
      for ((i = 0; i < 10; i++)); do
        sleep 1
        if has_chrome_window; then
          echo >&2 "← Chrome ready"
          break
        fi
      done
    fi

    echo >&2 "→ Running kick-off: $kick_off_sso_command"
    set +o pipefail
    # shellcheck disable=SC2029  # intentional client-side expansion
    ssh -A "$ssh_host" bash -c "\"$kick_off_sso_command\"" 2>&1 \
      | grep --line-buffered -m1 -oP 'https://\S+' \
      | {
          read -r url
          set -o pipefail
          if [[ -z "$url" ]]; then
            echo >&2 "✗ No URL found in kick-off output"
            exit 1
          fi
          echo >&2 "← URL: $url"

          port=$(grep -oP '127\.0\.0\.1%3A\K\d+' <<< "$url") || true
          if [[ -z "$port" ]]; then
            echo >&2 "✗ No port found in URL: $url"
            exit 1
          fi
          echo >&2 "→ Setting up SSH tunnel: localhost:$port → $ssh_host:$port"
          ssh -fL "$port:127.0.0.1:$port" "$ssh_host" sleep 120

          echo >&2 "→ Opening browser: $url"
          xdg-open "$url" || true
        }
    set -o pipefail

    echo >&2 "→ Running on-auth: $on_successful_auth_command"
    # shellcheck disable=SC2029  # intentional client-side expansion
    ssh -A "$ssh_host" bash -c "\"$on_successful_auth_command\""
    echo >&2 "← Done"
  '';
}
