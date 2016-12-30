{
  network.description = "'naberius' server in Hetzner";
  naberius =
    {config, pkgs, ...}: {
      imports = [
        ../modules/server-packages.nix
        ../modules/static-blog.nix
        ../modules/outgoing-email.nix
        ../modules/easyrsa.nix
      ];
      virtualisation.docker.enable = true;
      virtualisation.libvirtd.enable = true;
      services.fail2ban.enable = true;

      networking.firewall.enable = true;
      networking.firewall.logRefusedConnections = false;

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
          ifconfig = "10.10.10.1 10.10.10.2";
          ifconfig-pool = "10.10.10.128 10.10.10.254";
          route = "10.10.10.0 255.255.255.0";
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
