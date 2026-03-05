{
  self,
  lib,
  ...
}:
{
  flake.nixosModules.default =
    { ... }:
    {
      key = "nixos-config.modules.nixos.default";

      imports = [
        self.nixosModules.baseline
        self.nixosModules.emacs
        self.nixosModules.git
        self.nixosModules.interactive-cli
        self.nixosModules.sops
        self.nixosModules.tailscale
      ];
      config = {
        system.switch.enable = true;
        services.dbus.implementation = lib.mkDefault "broker";
      };
    };
}
