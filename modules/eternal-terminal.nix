{ self, ... }:
{
  nixosSharedModules = [ self.nixosModules.eternal-terminal ];

  flake.nixosModules.eternal-terminal =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.eternal-terminal";

      config = lib.mkIf config.hostConfig.feature.interactive-cli or false {
        services.eternal-terminal = {
          enable = true;
        };
        networking.firewall.allowedTCPPorts = [ config.services.eternal-terminal.port ];
      };
    };
}
