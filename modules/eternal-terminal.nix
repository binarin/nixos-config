{ ... }:
{

  flake.nixosModules.eternal-terminal =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.eternal-terminal";

      config = {
        services.eternal-terminal = {
          enable = config.services.openssh.enable;
        };
        networking.firewall.allowedTCPPorts = [ config.services.eternal-terminal.port ];
      };
    };
}
