{ ... }:
{

  flake.nixosModules.eternal-terminal =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.eternal-terminal";

      config = {
        services.eternal-terminal = {
          enable = true;
        };
        networking.firewall.allowedTCPPorts = [ config.services.eternal-terminal.port ];
      };
    };
}
