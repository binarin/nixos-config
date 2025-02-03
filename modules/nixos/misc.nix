{flake, lib, pkgs, config, ...}:
{
  config = {
    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    system.switch.enableNg = lib.mkDefault true;
    system.switch.enable = lib.mkDefault false;

    services.dbus.implementation = lib.mkDefault "broker";

    time.timeZone = lib.mkDefault "Europe/Amsterdam";
  };
}
