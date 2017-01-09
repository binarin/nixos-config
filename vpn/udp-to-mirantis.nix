{config, pkgs, ...}:
pkgs.lib.mkIf (builtins.pathExists "/root/.openvpn/udp-to-mirantis.key")
{
  up = "${pkgs.update-resolv-conf}/libexec/openvpn/update-resolv-conf";
  down = "${pkgs.update-resolv-conf}/libexec/openvpn/update-resolv-conf";
  autoStart = false;

  config = ''
client
dev tun
proto udp
fragment 1400
mssfix
#remote ra-kha.mirantis.com 443 #use this parameter if you connect to Kharkov
#remote ra-sar.mirantis.com 443 #use this parameter if you connect to Saratov
remote ra-msk.mirantis.com 443 #use this parameter if you connect to Moscow
#remote ra-sjc.mirantis.com 443 # use this parameter if you connect to San-Jose
#remote ra-poz.mirantis.com 443 #use this parameter if you connect to Poznan
resolv-retry infinite
nobind
float
persist-key
persist-tun
key-direction 1
auth-user-pass
comp-lzo
verb 3
mute-replay-warnings
ns-cert-type server
ca /root/.openvpn/udp-to-mirantis-ca.crt
tls-auth /root/.openvpn/udp-to-mirantis-tls.key
cert /root/.openvpn/udp-to-mirantis.crt
key /root/.openvpn/udp-to-mirantis.key
  '';
}
