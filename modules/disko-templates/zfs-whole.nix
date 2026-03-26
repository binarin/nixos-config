{ self, ... }:
{
  flake.nixosModules.disko-template-zfs-whole =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.disko-template-zfs-whole";
      imports = [
        self.nixosModules.impermanence
      ];

      config = lib.mkMerge [
        {
          disko.devices = {
            disk = {
              main = {
                type = "disk";
                device = lib.mkDefault (
                  throw "Set `disko.devices.disk.main.device` when importing disko-template-zfs-whole"
                );
                content = {
                  type = "gpt";
                  partitions = {
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
                };
              };
            };
          };
        }
        (lib.mkIf config.impermanence.enable {
          fileSystems."/persist".neededForBoot = true;
          fileSystems."/local".neededForBoot = true;
          disko.devices.zpool.rpool.datasets = {
            "ROOT/nixos".postCreateHook =
              "zfs list -t snapshot -H -o name | grep -E '^rpool/ROOT/nixos@blank$' || zfs snapshot rpool/ROOT/nixos@blank";
            "persist" = {
              type = "zfs_fs";
              mountpoint = "/persist";
            };
            "local" = {
              type = "zfs_fs";
              mountpoint = "/local";
            };
          };
        })
      ];
    };
}
