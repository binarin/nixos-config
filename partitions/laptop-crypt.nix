{...}:

{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = {
    laptop-crypt = { device="/dev/disk/by-partlabel/laptop-crypt"; allowDiscards = true; };
  };

  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos-root ";
      fsType = "ext4";
      options = [ "noatime" "discard" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-partlabel/nixos-boot";
      fsType = "vfat";
    };

  swapDevices = [ {device = "/dev/disk/by-label/swap-0"; } ];
}
