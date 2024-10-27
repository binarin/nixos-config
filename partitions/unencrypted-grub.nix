{
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  fileSystems."/" =
    {
      device = "/dev/disk/by-label/nixos-root";
      fsType = "ext4";
    };

  swapDevices =
    [{ device = "/dev/disk/by-label/swap-0"; }];
}
