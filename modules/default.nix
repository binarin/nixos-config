{
  self,
  lib,
  config,
  ...
}:
let
  flakeConfig = config;
in
{
  flake.nixosModules.default =
    { config, ... }:
    {
      key = "nixos-config.default";

      imports = [
        self.nixosModules.sshd
        self.nixosModules.zenburn
        self.nixosModules.tpm2-ssh
        self.nixosModules.nix
        self.nixosModules.inventory-legacy
        self.modules.generic.public-keys
      ];

      options = {
        inventory.hostIpAllocation = lib.mkOption {
          type = lib.types.raw;
          readOnly = true;
        };
      };

      config = {
        networking.hostId = (import "${self}/inventory/host-id.nix")."${config.networking.hostName}";
        networking.hosts = flakeConfig.inventory.networks.home.hosts;
        inventory.hostIpAllocation = flakeConfig.inventory.ipAllocation."${config.networking.hostName}";

        system.switch.enableNg = lib.mkDefault true;
        system.switch.enable = lib.mkDefault false;

        services.dbus.implementation = lib.mkDefault "broker";

        time.timeZone = lib.mkDefault "Europe/Amsterdam";
      };
    };
}
