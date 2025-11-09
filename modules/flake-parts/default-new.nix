{ self, lib, ... }:
{
  flake.nixosModules.default-new =
    { ... }:
    {
      key = "nixos-config.default-new";
      imports = [
        self.nixosModules.sshd
        self.nixosModules.tpm2-ssh
        self.nixosModules.nix
        self.sharedModules.public-keys
      ];

      config = {
        system.switch.enableNg = lib.mkDefault true;
        system.switch.enable = lib.mkDefault false;

        services.dbus.implementation = lib.mkDefault "broker";

        time.timeZone = lib.mkDefault "Europe/Amsterdam";
      };
    };
}
