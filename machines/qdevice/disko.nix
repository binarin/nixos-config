{lib, ...}:
{
  disko.devices = {
    disk = {
      main = {
        device = "/dev/disk/by-id/nvme-WD_Red_SN700_1000GB_241759800883";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              type = "EF00";
              size = "1G";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" "nofail" ];
              };
            };
            luks = {
              size = "100%";
              content = {
                type = "luks";
                name = "luks1";
                passwordFile = lib.mkForce "/tmp/password-without-newline";
                settings = {
                  allowDiscards = true;
                };
                content = {
                  type = "zfs";
                  pool = "rpool";
                };
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
