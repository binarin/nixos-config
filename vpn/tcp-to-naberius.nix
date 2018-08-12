{config, pkgs, ...}:
{
  autoStart = false;
  config = ''
    redirect-private
    client
    cipher AES-128-CBC
    dev ovpn-naberius
    dev-type tun

    <connection>
      remote naberius.binarin.ru 8443 tcp
    </connection>

    <connection>
      remote binarin.ru 8443 tcp
      http-proxy ${builtins.replaceStrings ["baz" "lol" "inc" "queen"] ["boo" "web" "corp" "king"] "lolproxy.corp.bazqueen.com"} 3128
    </connection>

    resolv-retry infinite
    nobind
    user nobody
    group nogroup
    persist-key
    persist-tun
    ca /root/.openvpn/${config.networking.hostName}-to-naberius-tcp-server-ca.crt
    cert /root/.openvpn/${config.networking.hostName}-to-naberius-tcp-server.crt
    key /root/.openvpn/${config.networking.hostName}-to-naberius-tcp-server.key
    remote-cert-tls server
    comp-lzo
    verb 3
    tun-mtu 1300
  '';
}
