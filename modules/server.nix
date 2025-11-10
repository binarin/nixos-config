{ self, ... }:
{
  nixosSharedModules = [ self.nixosModules.server ];

  flake.nixosModules.server =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.server";

      config = lib.mkIf config.hostConfig.feature.server or false {
        # services.fail2ban.enable = true;
        # services.openssh.enable = true;
      };
    };
}
