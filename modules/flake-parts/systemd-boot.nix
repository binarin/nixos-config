{flake, ...}: {

  flake.nixosModules.systemd-boot = {...}: {
    key = "nixos-config.systemd-boot";
    config = {
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
    };
  };

}
