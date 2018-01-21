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
        default = [ "enp3s0" ];
        type = types.listOf types.str;
      };
    };
  };
  config = mkIf cfg.enable {
    services.rabbitmq = {
      enable = true;
      listenAddress = "0.0.0.0";
      config = ''
      '';
      plugins = [ "rabbitmq_management" "rabbitmq_mqtt" ];
    };
    networking.firewall.extraCommands = concatStringsSep "\n" (map (
      iface: ''
        ip46tables -A nixos-fw -i ${iface} -p tcp -m multiport --dports 15672,5672,1883 -j nixos-fw-accept
      ''
      ) cfg.trustedInterfaces
    );
  };
}
