{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              priority = 1;
              name = "ESP";
              start = "1M";
              end = "128M";
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
                type = "zfs";
                pool = "rpool";
              };
            };
          };
        };
      };
    };
    zpool = {
      rpool = {
        type = "zpool";
        options = {
          ashift = "12";
          autotrim = "on";
        };

        rootFsOptions = {
          mountpoint = "/";
          acltype = "posixacl";
          xattr = "sa";
          dnodesize = "auto";
          compression = "lz4";
          normalization = "formD";
          relatime = "on";
          canmount = "off";
        };

        datasets = {
          "ROOT" = {
            type = "zfs_fs";
            options = {
              canmount = "off";
              mountpoint = "none";
            };
          };
          "ROOT/nixos" = {
            type = "zfs_fs";
            options = {
              canmount = "noauto";
              mountpoint = "/";
            };
            mountpoint = "/";
            postCreateHook = "zfs list -t snapshot -H -o name | grep -E '^rpool/ROOT/nixos@blank$' || zfs snapshot rpool/ROOT/nixos@blank";
          };
          "var" = {
            type = "zfs_fs";
            options = {
              canmount = "off";
            };
          };
          "var/lib" = {
            type = "zfs_fs";
            options = {
              canmount = "off";
            };
          };
          "var/lib/docker" = {
            type = "zfs_fs";
            options = {
              "com.sun:auto-snapshot" = "false";
            };
            mountpoint = "/var/lib/docker";
          };
          "var/log" = {
            type = "zfs_fs";
            mountpoint = "/var/log";
          };
          "nix" = {
            type = "zfs_fs";
            mountpoint = "/nix";
          };
          "persist" = {
            type = "zfs_fs";
            mountpoint = "/persist";
          };
          "local" = {
            type = "zfs_fs";
            mountpoint = "/local";
          };
        };
      };
    };
  };
}
