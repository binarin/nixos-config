{config, lib, pkgs, ...}:

with lib;
let
  cfg = config.services.pi-hole;
in
{
  options = {
    services.pi-hole = {
      enable = mkEnableOption "pi-hole";
      bindIp = mkOption {
        type = types.str;
        default = "0.0.0.0";
      };
      dnsMasqConfig = mkOption {
        type = types.str;
        default = "";
      };
    };
  };

  config = mkIf cfg.enable {
    virtualisation.docker.enable = true;

    services.dnsmasq.enable = false; # as it binds to 0.0.0.0

    systemd.services."pi-hole" = let
      stateDir = "/var/lib/pihole/";
      script = pkgs.writeScript "pi-hole-in-docker" ''
        #!${pkgs.bash}/bin/bash

        rm -f ${stateDir}/dnsmasq.d/local.conf
        cp ${pkgs.writeText "local.conf" cfg.dnsMasqConfig} ${stateDir}/dnsmasq.d/local.conf

        docker run --rm -t \
        --name pihole \
        -p ${cfg.bindIp}:53:53/tcp -p ${cfg.bindIp}:53:53/udp \
        -p 30080:80 \
        -p 30443:443 \
        -v "${stateDir}/pihole/:/etc/pihole/" \
        -v "${stateDir}/dnsmasq.d/:/etc/dnsmasq.d/" \
        --cap-add=NET_ADMIN \
        --dns=127.0.0.1 --dns=1.1.1.1 \
        pihole/pihole:latest
      '';
    in {
      description = "Starts pi-hole docker container";
      after = [ "docker.service "];
      wants = [ "wants.service "];
      wantedBy = [ "default.target" ];
      path = [ pkgs.docker ];
      serviceConfig = {
        ExecStart = script;
      };
    };
  };
}
