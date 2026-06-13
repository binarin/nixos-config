{
  writeShellApplication,
  systemd,
  coreutils,
  gnugrep,
  openssh,
  xdg-utils,
  protectedVars,
}:
let
  protectedArray = "(${builtins.concatStringsSep " " protectedVars})";
in
writeShellApplication {
  name = "ksso";
  runtimeInputs = [ systemd coreutils gnugrep openssh xdg-utils ];
  text = ''
    if [[ $# -ne 2 ]]; then
      echo >&2 "Usage: ksso <kick-off-sso-command> <on-successful-auth-command>"
      exit 1
    fi
    kick_off_sso_command="$1"
    on_successful_auth_command="$2"

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
      printf -v current_val '%s' "''${!var}"
      if [[ "$current_val" != "$val" ]]; then
        export "$var"="$val"
      fi
    done < <(systemctl show-environment --user 2>/dev/null)

    set -euo pipefail

    ssh_host="adb.k.b"

    # shellcheck disable=SC2029  # intentional client-side expansion
    url=$(ssh "$ssh_host" "bash -c '$kick_off_sso_command'" 2>&1 \
      | grep --line-buffered -m1 -oP 'https://\S+')

    port=$(grep -oP '127\.0\.0\.1%3A\K\d+' <<< "$url")

    ssh -fL "$port:127.0.0.1:$port" "$ssh_host" sleep 120

    xdg-open "$url"

    # shellcheck disable=SC2029  # intentional client-side expansion
    ssh "$ssh_host" "bash -c '$on_successful_auth_command'"
  '';
}
