{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.xray-share = pkgs.writeShellApplication {
        name = "xray-share";
        runtimeInputs = [ pkgs.jq pkgs.qrencode pkgs.sops pkgs.git pkgs.gnused ];
        text = ''
          set -euo pipefail
          machine="xray-front"
          json=""
          user=""
          while [ "$#" -gt 0 ]; do
            case "$1" in
              --json) json="$2"; shift 2 ;;
              --machine) machine="$2"; shift 2 ;;
              -*) echo "unknown flag: $1" >&2; exit 2 ;;
              *) user="$1"; shift ;;
            esac
          done

          if [ -z "$json" ]; then
            [ -n "$user" ] || { echo "usage: xray-share <user> | xray-share --json <file>" >&2; exit 2; }
            root="$(git rev-parse --show-toplevel)"
            secret="$root/vars/per-machine/$machine/xray-user-$user/client.json/secret"
            [ -f "$secret" ] || { echo "no client.json for user '$user' (looked at $secret)" >&2; exit 1; }
            content="$(sops decrypt "$secret")"
          else
            content="$(cat "$json")"
            [ -n "$user" ] || user="xray"
          fi

          proxy="$(printf '%s' "$content" | jq -c '.outbounds[] | select(.tag == "proxy")')"
          id="$(printf '%s' "$proxy" | jq -r '.settings.vnext[0].users[0].id')"
          host="$(printf '%s' "$proxy" | jq -r '.settings.vnext[0].address')"
          port="$(printf '%s' "$proxy" | jq -r '.settings.vnext[0].port')"
          rs="$(printf '%s' "$proxy" | jq -c '.streamSettings.realitySettings')"
          sni="$(printf '%s' "$rs" | jq -r '.serverName')"
          pbk="$(printf '%s' "$rs" | jq -r '.publicKey')"
          sid="$(printf '%s' "$rs" | jq -r '.shortId')"

          url="vless://$id@$host:$port?type=raw&security=reality&pbk=$pbk&fp=chrome&sni=$sni&sid=$sid&flow=xtls-rprx-vision&spx=%2F#$user"
          printf '%s\n' "$url"
          qrencode -t ANSIUTF8 "$url"
        '';
      };
    };
}
