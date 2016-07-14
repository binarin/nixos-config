{ config, lib, pkgs, ... }:

{
  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos-root";
    fsType = "btrfs";
    options = [ "ssd" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-partlabel/uefi-boot";
    fsType = "vfat";
  };

  swapDevices = [ {device = "/dev/disk/by-label/swap-0"; } ];
}
