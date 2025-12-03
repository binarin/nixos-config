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

      config = {
        system.switch.enable = true;

        services.dbus.implementation = lib.mkDefault "broker";

        time.timeZone = lib.mkDefault "Europe/Amsterdam";
      };
    };
}
