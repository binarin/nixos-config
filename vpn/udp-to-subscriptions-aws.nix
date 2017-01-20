{config, pkgs, ...}:
pkgs.lib.mkIf (builtins.pathExists "/root/.openvpn/subscriptions-aws.key") {
  autoStart = false;
  config = ''
dev tun
proto udp
remote 54.76.20.78
port 443
client
resolv-retry infinite
ca /root/.openvpn/subscriptions-aws-ca.crt
cert /root/.openvpn/subscriptions-aws.crt
key /root/.openvpn/subscriptions-aws.key
tls-client
tls-auth /root/.openvpn/subscriptions-aws.tls-key 1
auth MD5
cipher BF-CBC
ns-cert-type server
comp-lzo
persist-key
persist-tun
verb 3
  '';
}
