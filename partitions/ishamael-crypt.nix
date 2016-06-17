{...}:

{
  boot.initrd.luks.devices = [ { name = "ishamael-crypt"; device="/dev/disk/by-partlabel/ishamael-crypt"; allowDiscards = true; } ];

  fileSystems."/" =
    { device = "/dev/ishamael/root";
      fsType = "ext4";
      options = [ "noatime" "discard" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-partlabel/ishamael-boot";
      fsType = "vfat";
    };

  swapDevices = [ {device = "/dev/disk/by-label/ishamael-swap"; } ];
}
