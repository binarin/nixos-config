{
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos-root";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-label/nixos-boot";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-label/swap-0"; }
      { device = "/dev/disk/by-label/swap-1"; }
      { device = "/dev/disk/by-label/swap-2"; }
      { device = "/dev/disk/by-label/swap-3"; }
    ];
}
