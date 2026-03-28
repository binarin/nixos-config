{ ... }:
{
  flake.nixosModules.disko-template-add-uefi-boot-to-main =
    { lib, ... }:
    {
      key = "nixos-config.modules.nixos.disko-template-add-uefi-boot-to-main";
      disko.devices.disk.main.content.partitions.ESP = {
        priority = 1;
        name = "ESP";
        start = "1M";
        size = "512M";
        type = "EF00";
        content = {
          type = "filesystem";
          format = "vfat";
          mountpoint = "/boot";
          mountOptions = [ "umask=0077" ];
        };
      };
    };
}
