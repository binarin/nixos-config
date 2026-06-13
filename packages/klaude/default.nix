{
  writeShellApplication,
  openssh,
  privoxy,
  coreutils,
  procps,
}:
writeShellApplication {
  name = "klaude";
  runtimeInputs = [ openssh privoxy coreutils procps ];
  text = ''
    if [[ $# -lt 1 ]]; then
      echo >&2 "Usage: klaude <ssh-host> [claude-args...]"
      exit 1
    fi

    ssh_host="$1"
    shift

    pick_free_port() {
      local port
      while true; do
        port=$(shuf -i 20000-65000 -n 1)
        if ! ss -tln | grep -q ":$port "; then
          echo "$port"
          return
        fi
      done
    }

    socks_port=$(pick_free_port)
    http_port=$(pick_free_port)

    echo "SOCKS5 proxy on localhost:$socks_port"
    echo "HTTP proxy on localhost:$http_port"

    ssh -N -D "$socks_port" "$ssh_host" &
    ssh_pid=$!

    privoxy --no-daemon <(echo -e "forward-socks5 / 127.0.0.1:$socks_port .\nlisten-address 127.0.0.1:$http_port") &
    privoxy_pid=$!

    sleep 0.5

    cleanup() {
      kill "$privoxy_pid" 2>/dev/null
      kill "$ssh_pid" 2>/dev/null
      wait "$privoxy_pid" 2>/dev/null
      wait "$ssh_pid" 2>/dev/null
    }
    trap cleanup EXIT

    HTTP_PROXY="http://127.0.0.1:$http_port" \
    HTTPS_PROXY="http://127.0.0.1:$http_port" \
    http_proxy="http://127.0.0.1:$http_port" \
    https_proxy="http://127.0.0.1:$http_port" \
    exec claude "$@"
  '';
}
