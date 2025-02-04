{
  disko.devices = {
    disk = {
      root = {
        type = "disk";
        device = "/dev/disk/by-id/ata-Samsung_SSD_870_EVO_1TB_S75CNX0XC06201Y";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "1G";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
            demandred-luks-lvm = {
              size = "100%";
              content = {
                type = "luks";
                name = "demandred-lvm";
                passwordFile = "/tmp/luks.pass";
                settings = {
                  allowDiscards = true;
                };
                content = {
                  type = "lvm_pv";
                  vg = "main";
                };
              };
            };
          };
        };
      };
    };
    lvm_vg = {
      main = {
        type = "lvm_vg";
        lvs = {
          all = {
            size = "100%";
            content = {
              type = "btrfs";
              subvolumes = {
                "/root" = {
                  mountpoint = "/";
                  mountOptions = [ "compress=zstd" ];
                };
                "/nix" = {
                  mountpoint = "/nix";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "/persist/all" = {
                  mountpoint = "/persist";
                  mountOptions = [ "compress=zstd" ];
                };
                "/local/all" = {
                  mountpoint = "/local";
                  mountOptions = [ "compress=zstd" ];
                };
                "/persist/binarin/personal-workspace" = {
                  mountpoint = "/home/binarin/personal-workspace";
                  mountOptions = [ "compress=zstd" ];
                };
              };
            };
          };
          swap = {
            size = "17G";
            content = {
              type = "swap";
              resumeDevice = true;
            };
          };
        };
      };
    };
  };
}
