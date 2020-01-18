{config, lib, pkgs, ...}:
with lib;

let
  cfg = config.my-vpn-server;

in
{
  options.my-vpn-server = {
    enable = lib.mkEnableOption "my-vpn-server";
    tcpNet = mkOption {
      type = types.str;
      default = "172.25.14";
    };

    udpNet = mkOption {
      type = types.str;
      default = "172.25.15";
    };

    externalIface = mkOption {
      type = types.str;
      default = "br0";
    };
  };

  config = mkIf cfg.enable {
    networking = {
      nat = {
        enable = true;
        internalInterfaces = ["ovpn-server-tcp"];
        externalInterface = cfg.externalIface;
      };

      firewall.allowedTCPPorts = [
        80 443 8443
      ];

      firewall.extraCommands = ''
        ip46tables -A FORWARD -i ovpn-server-tcp -j ACCEPT || true
        ip46tables -A FORWARD -o ovpn-server-tcp -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT || true

        ip46tables -A FORWARD -p all -i br0 -j ACCEPT || true
      '';

      firewall.extraStopCommands = ''
        ip46tables -D FORWARD -i ovpn-server-tcp -j ACCEPT || true
        ip46tables -D FORWARD -o ovpn-server-tcp -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT || true

        ip46tables -D FORWARD -p all -i br0 -j ACCEPT || true
      '';

    };

    services.nginx.enable = true;
    services.nginx.virtualHosts."vpn.binarin.ru" = {
      forceSSL = true;
      enableACME = true;
    };

    security.acme.certs."vpn.binarin.ru" = {
      postRun = ''
        systemctl restart openvpn-tcp-server-with-password.service
      '';
    };

    services.openvpn.servers.tcp-server-with-password = {
      config = ''
        mode server
        dh none
        tls-server
        dev-type tun
        dev ovpn-server-tcp
        port 8443
        proto tcp-server
        cert /var/lib/acme/vpn.binarin.ru/fullchain.pem
        key /var/lib/acme/vpn.binarin.ru/key.pem
        ca /etc/ssl/certs/ca-bundle.crt
        plugin openvpn-plugin-auth-pam.so "login login USERNAME password PASSWORD"
        verify-client-cert none

        cipher AES-128-CBC
        client-to-client

        ifconfig ${cfg.tcpNet}.1 ${cfg.tcpNet}.2
        ifconfig-pool ${cfg.tcpNet}.128 ${cfg.tcpNet}.254

        keepalive 5 30
        tun-mtu 1300

        push "route ${cfg.tcpNet}.0 255.255.255.0"
        push "topology net30"

        route ${cfg.tcpNet}.0 255.255.255.0

        push "route 0.0.0.0 248.0.0.0"
        push "route 8.0.0.0 254.0.0.0"
        push "route 11.0.0.0 255.0.0.0"
        push "route 12.0.0.0 252.0.0.0"
        push "route 16.0.0.0 240.0.0.0"
        push "route 32.0.0.0 224.0.0.0"
        push "route 64.0.0.0 192.0.0.0"
        push "route 128.0.0.0 224.0.0.0"
        push "route 160.0.0.0 248.0.0.0"
        push "route 168.0.0.0 252.0.0.0"
        push "route 172.0.0.0 255.240.0.0"
        push "route 172.32.0.0 255.224.0.0"
        push "route 172.64.0.0 255.192.0.0"
        push "route 172.128.0.0 255.128.0.0"
        push "route 173.0.0.0 255.0.0.0"
        push "route 174.0.0.0 254.0.0.0"
        push "route 176.0.0.0 240.0.0.0"
        push "route 192.0.0.0 255.128.0.0"
        push "route 192.128.0.0 255.224.0.0"
        push "route 192.160.0.0 255.248.0.0"
        push "route 192.169.0.0 255.255.0.0"
        push "route 192.170.0.0 255.254.0.0"
        push "route 192.172.0.0 255.252.0.0"
        push "route 192.176.0.0 255.240.0.0"
        push "route 192.192.0.0 255.192.0.0"
        push "route 193.0.0.0 255.0.0.0"
        push "route 194.0.0.0 254.0.0.0"
        push "route 196.0.0.0 252.0.0.0"
        push "route 200.0.0.0 248.0.0.0"
        push "route 208.0.0.0 240.0.0.0"
      '';
    };
  };

}
