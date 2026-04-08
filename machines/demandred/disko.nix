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

            swap = {
              size = "17G";
              content = {
                type = "swap";
                resumeDevice = true;
              };
            };

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
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                  };
                  "/persist/all" = {
                    mountpoint = "/persist";
                    mountOptions = [ "compress=zstd" ];
                  };
                  "/local/all" = {
                    mountpoint = "/local";
                    mountOptions = [ "compress=zstd" ];
                  };
                  "/var/lib/docker" = {
                    mountpoint = "/var/lib/docker";
                    mountOptions = [ "compress=zstd" ];
                  };
                };
              };
            };
          };
        };
      };
    };
  };
}
