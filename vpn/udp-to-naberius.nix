{config, ...}:

{
  config = ''
    client
    dev tun
    proto udp
    remote naberius.binarin.ru 1194
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
