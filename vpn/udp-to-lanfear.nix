{config, pkgs, ...}:
{
  autoStart = false;
  config = ''
    errors-to-stderr

    cipher AES-128-CBC

    dev ovpu-lanfear
    dev-type tun
    client

    <connection>
      remote org.binarin.ru 8443 udp
      tun-mtu 1300
      nobind
    </connection>

    resolv-retry infinite
    user nobody
    group nogroup
    persist-key
    persist-tun

    ca /etc/ssl/certs/ca-bundle.crt
    auth-user-pass /root/.lanfear-vpn-user-pass
    verify-x509-name CN=org.binarin.ru
  '';
}
