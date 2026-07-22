{ ... }:
{
  disko.devices.disk.main = {
    type = "disk";
    # Proxmox scsi0 (virtio-scsi-single). The disko image build partitions this
    # by-id path; at runtime the ext4 root is mounted by partlabel
    # (disko-main-root), so no build-time device path or hostid is needed.
    device = "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0";
    # Raw diskoImages size (ncf provision-vm). disko's default is 2G; size it to
    # hold the ~4.6G closure. Bump if the system grows.
    imageSize = "6G";
    content = {
      type = "gpt";
      partitions = {
        ESP = {
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
        root = {
          size = "100%";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/";
          };
        };
      };
    };
  };
}
