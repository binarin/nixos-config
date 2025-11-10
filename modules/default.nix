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
        self.nixosModules.emacs
        self.nixosModules.eternal-terminal
        self.nixosModules.git
        self.nixosModules.interactive-cli
        self.nixosModules.inventory-legacy
        self.nixosModules.nix
        self.nixosModules.security
        self.nixosModules.sops
        self.nixosModules.sshd
        self.nixosModules.tailscale
        self.nixosModules.use-nix-cache
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
