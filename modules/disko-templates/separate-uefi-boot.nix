{ ... }:
{
  flake.nixosModules.disko-template-separate-uefi-boot =
    { lib, ... }:
    {
      key = "nixos-config.modules.nixos.disko-template-separate-uefi-boot";
      disko.devices.disk.boot = {
        type = "disk";
        device = lib.mkDefault (
          throw "Set `disko.devices.disk.boot.device` when importing disko-template-separate-uefi-boot"
        );
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              priority = 1;
              name = "ESP";
              start = "1M";
              size = "100%";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
          };
        };
      };
    };
}
