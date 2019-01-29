{config, pkgs, ...}:
{
  autoStart = false;
  config = ''
    cipher AES-128-CBC

    dev ovpn-lanfear-tcp
    dev-type tun
    client

    redirect-private

    <connection>
      remote org.binarin.ru 8443 tcp
      http-proxy ${builtins.replaceStrings ["baz" "lol" "inc" "queen"] ["boo" "web" "corp" "king"] "lolproxy.corp.bazqueen.com"} 3128
      tun-mtu 1300
    </connection>

    <connection>
      remote org.binarin.ru 8443 tcp
      tun-mtu 1300
    </connection>

    resolv-retry infinite
    nobind
    user nobody
    group nogroup
    persist-key
    persist-tun

    ca /etc/ssl/certs/ca-bundle.crt
    auth-user-pass
    verify-x509-name CN=org.binarin.ru
  '';
}
