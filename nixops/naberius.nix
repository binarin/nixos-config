let
  vpnIp = "10.10.10.1";
  vpnNet = "10.10.10.0/24";
  vpnIface = "ovpn-server";
in {
  network.description = "'naberius' server in Hetzner";
  naberius =
    {config, pkgs, ...}: {
      imports = [
        ../modules/server-packages.nix
        ../modules/static-blog.nix
        ../modules/outgoing-email.nix
        ../modules/easyrsa.nix
        ../modules/squid.nix
      ];
      virtualisation.docker.enable = true;
      virtualisation.libvirtd.enable = true;
      services.fail2ban.enable = true;

      services.squid.enable = true;
      services.squid.config = ''
        http_port ${vpnIp}:3128
        # Example rule allowing access from your local networks.
        # Adapt to list your (internal) IP networks from where browsing
        # should be allowed
        acl localnet src ${vpnNet}

        acl SSL_ports port 443
        acl Safe_ports port 80		# http
        acl Safe_ports port 21		# ftp
        acl Safe_ports port 443		# https
        acl Safe_ports port 70		# gopher
        acl Safe_ports port 210		# wais
        acl Safe_ports port 1025-65535	# unregistered ports
        acl Safe_ports port 280		# http-mgmt
        acl Safe_ports port 488		# gss-http
        acl Safe_ports port 591		# filemaker
        acl Safe_ports port 777		# multiling http
        acl CONNECT method CONNECT

        #
        # Recommended minimum Access Permission configuration:
        #
        # Deny requests to certain unsafe ports
        http_access deny !Safe_ports

        # Deny CONNECT to other than secure SSL ports
        http_access deny CONNECT !SSL_ports

        # Only allow cachemgr access from localhost
        http_access allow localhost manager
        http_access deny manager

        # We strongly recommend the following be uncommented to protect innocent
        # web applications running on the proxy server who think the only
        # one who can access services on "localhost" is a local user
        #http_access deny to_localhost

        #
        # INSERT YOUR OWN RULE(S) HERE TO ALLOW ACCESS FROM YOUR CLIENTS
        #

        # Example rule allowing access from your local networks.
        # Adapt localnet in the ACL section to list your (internal) IP networks
        # from where browsing should be allowed
        http_access allow localnet
        http_access allow localhost

        # And finally deny all other access to this proxy
        http_access deny all


        #
        # Add any of your own refresh_pattern entries above these.
        #
        refresh_pattern ^ftp:		1440	20%	10080
        refresh_pattern ^gopher:	1440	0%	1440
        refresh_pattern -i (/cgi-bin/|\?) 0	0%	0
        refresh_pattern .		0	20%	4320
   '';

      networking.firewall.enable = true;
      networking.firewall.logRefusedConnections = false;
      networking.firewall.extraCommands = ''
        ip46tables -A nixos-fw -i ${vpnIface} -p tcp --dport 3128 -j nixos-fw-accept
      '';

      services.openvpn-easyrsa.servers.test = {
        ca = {
          reqCountry = "RU";
          reqProvince = "Moscow";
          reqCity = "Moscow";
          reqOrg = "binarin.ru";
          reqEmail = "binarin@binarin.ru";
          reqOu = "binarin.ru";
        };
        port = 2149;
        proto = "udp";
        config = {
          dev = "ovpn-server";
          dev-type = "tun";
          cipher = "AES-128-CBC";
          mode = "server";
          tls-server = true;
          push = [
            "\"route 10.10.10.0 255.255.255.0\""
            "\"topology net30\""
          ];
          client-to-client = true;
          ifconfig = "${vpnIp} 10.10.10.2";
          ifconfig-pool = "10.10.10.128 10.10.10.254";
          route = "10.10.10.0 255.255.255.0";
          keepalive = "10 60";
        };
        clients = {
         demandred-to-naberius-udp-server = {
           ccd = {
             ifconfig-push = "10.10.10.6 10.10.10.5";
           };
         };
         lanfear-to-naberius-udp-server = {
           ccd = {
             ifconfig-push = "10.10.10.10 10.10.10.9";
           };
         };
         ishamael-to-naberius-udp-server = {
           ccd = {
             ifconfig-push = "10.10.10.14 10.10.10.13";
           };
         };
      };
   };
 };
}
