{config, pkgs, ...}:
pkgs.lib.mkIf (builtins.pathExists "/root/.openvpn/${config.networking.hostName}-to-amon-udp-server.key")
{
  config = ''
    client
    cipher AES-128-CBC
    dev ovpn-naberius
    dev-type tun
    proto udp
    remote amon.binarin.ru 2149
    resolv-retry infinite
    nobind
    user nobody
    group nogroup
    persist-key
    persist-tun
    ca /root/.openvpn/${config.networking.hostName}-to-naberius-udp-server-ca.crt
    cert /root/.openvpn/${config.networking.hostName}-to-naberius-udp-server.crt
    key /root/.openvpn/${config.networking.hostName}-to-naberius-udp-server.key
    ns-cert-type server
    comp-lzo
    verb 3
  '';
}
