{ lib, config, pkgs, ... }: {
  config = {

    services.grocy = {
      enable = true;
      hostName = "grocy.binarin.info";
      nginx.enableSSL = false;
      settings = {
        currency = "EUR";
        culture = "nl";
        calendar.firstDayOfWeek = 1;
      };
    };

    services.nginx.virtualHosts."grocy.binarin.info".listen = [
      {
        adress = "127.0.0.1";
        port = "64084";
      }
    ];

    services.caddy.virtualHosts."grocy.binarin.info".extraConfig = ''
      reverse_proxy http://127.0.0.1:64084

      import letsencrypt
    '';
  };
}
