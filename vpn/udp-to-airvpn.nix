{config, ...}:

{
  autoStart = false;
  config = ''
    client
    dev tun
    proto udp
    remote europe.vpn.airdns.org 443
    resolv-retry infinite
    nobind
    persist-key
    persist-tun
    remote-cert-tls server
    cipher AES-256-CBC
    comp-lzo no
    route-delay 5
    verb 3
    explicit-exit-notify 5

    ca /root/.openvpn/${config.networking.hostName}-to-airvpn-udp-server-ca.crt
    cert /root/.openvpn/${config.networking.hostName}-to-airvpn-udp-server.crt
    key /root/.openvpn/${config.networking.hostName}-to-airvpn-udp-server.key
    key-direction 1
    tls-auth /root/.openvpn/${config.networking.hostName}-to-airvpn-udp-server.tls
  '';
}
