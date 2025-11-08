{ config, lib, ... }:
{
  config = lib.mkIf config.hostConfig.feature.interactive-cli {
    services.eternal-terminal = {
      enable = true;
    };
    networking.firewall.allowedTCPPorts = [ config.services.eternal-terminal.port ];
  };
}
