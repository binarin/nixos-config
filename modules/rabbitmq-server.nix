{config, lib, pkgs, ...}:

with lib;

let
  cfg = config.services.rabbitmq-server;

in {
  options = {
    services.rabbitmq-server = {
      enable = mkOption {
        default = true;
      };
      trustedInterfaces = mkOption {
        default = [ "enp3s0" "lxdbr2" ];
        type = types.listOf types.str;
      };
    };
  };

  imports = [
    ../nixpkgs-master/nixos/modules/services/networking/epmd.nix
    ../nixpkgs-proposed/nixos/modules/services/amqp/rabbitmq.nix
  ];

  disabledModules = [ "services/amqp/rabbitmq.nix" ];

  config = mkIf cfg.enable {
    users.extraUsers.rabbitmq.extraGroups = [ "acme-reader" ];

    services.epmd.package = pkgs.bleeding.erlangR20;

    services.rabbitmq = {
      enable = true;
      package = pkgs.proposed.rabbitmq-server;
      plugins = [ "rabbitmq_management" "rabbitmq_mqtt" ];
      listenAddress = "0.0.0.0";
      configItems = {
        "management.listener.port" = "15672";
        "management.listener.ssl" = "true";
        "management.listener.ssl_opts.cacertfile" = "/var/lib/acme/amon.binarin.ru/fullchain.pem";
        "management.listener.ssl_opts.certfile" = "/var/lib/acme/amon.binarin.ru/fullchain.pem";
        "management.listener.ssl_opts.keyfile" = "/var/lib/acme/amon.binarin.ru/key.pem";
        "mqtt.allow_anonymous" = "false";
        "mqtt.listeners.ssl.1" = "8883";
        "ssl_options.cacertfile" = "/var/lib/acme/amon.binarin.ru/fullchain.pem";
        "ssl_options.certfile" = "/var/lib/acme/amon.binarin.ru/fullchain.pem";
        "ssl_options.keyfile" = "/var/lib/acme/amon.binarin.ru/key.pem";
      };
    };

    networking.firewall.extraCommands = concatStringsSep "\n" (map (
      iface: ''
        ip46tables -A nixos-fw -i ${iface} -p tcp -m multiport --dports 15672,5672,1883 -j nixos-fw-accept
        ip46tables -A nixos-fw -p tcp --dport 8883 -j nixos-fw-accept
      ''
      ) cfg.trustedInterfaces
    );
  };
}
