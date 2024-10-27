{
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  boot.initrd.luks.devices = [{ name = "laptop-crypt"; device = "/dev/sda2"; allowDiscards = true; }];

  fileSystems."/" =
    {
      device = "/dev/disk/by-label/nixos-root";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-label/nixos-boot";
      fsType = "ext4";
    };

  swapDevices =
    [{ device = "/dev/disk/by-label/swap-0"; }];
}
