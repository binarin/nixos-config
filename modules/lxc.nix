{ config, ... }:
let
  flakeConfig = config;
in
{
  flake.nixosModules.lxc =
    {
      modulesPath,
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.lxc";

      # Need to add to /etc/pve/lxc/XXX.conf
      # lxc.cgroup2.devices.allow: c 10:200 rwm
      # lxc.mount.entry: /dev/net/tun dev/net/tun none bind,create=file
      imports = [
        "${modulesPath}/virtualisation/proxmox-lxc.nix"
      ];

      options.proxmoxLXC = {
        tarballCompression = lib.mkOption {
          type = lib.types.enum [
            "xz"
            "zstd"
          ];
          default = "zstd";
          description = "Compression format for LXC tarball (zstd is ~12x faster, xz for PVE < 7.1)";
        };

        cores = lib.mkOption {
          type = lib.types.int;
          default = 2;
          description = "Number of CPU cores";
        };

        memory = lib.mkOption {
          type = lib.types.int;
          default = 2048;
          description = "Memory allocation in MB";
        };

        swap = lib.mkOption {
          type = lib.types.int;
          default = 0;
          description = "Swap allocation in MB";
        };

        onboot = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Start container on boot";
        };

        unprivileged = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Run as unprivileged container";
        };

        rootfs = lib.mkOption {
          type = lib.types.submodule {
            options = {
              pool = lib.mkOption {
                type = lib.types.str;
                default = "local-zfs";
                description = "Storage pool name for root filesystem";
              };
              size = lib.mkOption {
                type = lib.types.str;
                default = "32G";
                description = "Root filesystem size";
              };
            };
          };
          default = { };
          description = "Root filesystem configuration";
        };

        mounts = lib.mkOption {
          type = lib.types.listOf (
            lib.types.submodule {
              options = {
                pool = lib.mkOption {
                  type = lib.types.str;
                  default = "local-zfs";
                  description = "Storage pool name";
                };
                mountPoint = lib.mkOption {
                  type = lib.types.str;
                  description = "Mount point inside container";
                };
                size = lib.mkOption {
                  type = lib.types.str;
                  description = "Volume size (e.g., '2T', '128G')";
                };
                backup = lib.mkOption {
                  type = lib.types.bool;
                  default = true;
                  description = "Include in backups";
                };
                replicate = lib.mkOption {
                  type = lib.types.bool;
                  default = true;
                  description = "Include in replication";
                };
              };
            }
          );
          default = [ ];
          description = "Additional mount points";
        };

        extraConfig = lib.mkOption {
          type = lib.types.lines;
          default = ''
            lxc.cgroup2.devices.allow: c 10:200 rwm
            lxc.mount.entry: /dev/net/tun dev/net/tun none bind,create=file
          '';
          description = "Extra lines to append to /etc/pve/lxc/<vmid>.conf (Tailscale enabled by default)";
        };
      };

      config = {
        proxmoxLXC.enable = true;

        proxmoxLXC.manageNetwork = true;
        networking.useNetworkd = true;
        networking.useHostResolvConf = false;
        networking.useDHCP = false;

        # Override tarball building based on compression format
        image.extension = lib.mkForce (
          if config.proxmoxLXC.tarballCompression == "zstd" then "tar.zst" else "tar.xz"
        );

        system.build.tarball = lib.mkForce (
          pkgs.callPackage "${modulesPath}/../lib/make-system-tarball.nix" {
            fileName = config.image.baseName;
            storeContents = [
              {
                object = config.system.build.toplevel;
                symlink = "none";
              }
            ];
            contents = [
              {
                source = config.system.build.toplevel + "/init";
                target = "/sbin/init";
              }
            ];
            extraCommands = "mkdir -p root etc/systemd/network";
            compressCommand = if config.proxmoxLXC.tarballCompression == "zstd" then "zstd -T0" else "pixz -t";
            compressionExtension = if config.proxmoxLXC.tarballCompression == "zstd" then ".zst" else ".xz";
            extraInputs =
              if config.proxmoxLXC.tarballCompression == "zstd" then [ pkgs.zstd ] else [ pkgs.pixz ];
          }
        );

        systemd.network.networks."40-lxc" = {
          matchConfig.Name = "eth0";
          dns = flakeConfig.inventory.networks.home.dns;
          address = [
            flakeConfig.inventory.ipAllocation."${config.networking.hostName}".home.primary.addressWithPrefix
          ];
          routes = [ { Gateway = flakeConfig.inventory.networks.home.gateway; } ];
        };

        services.getty.autologinUser = "root";

        # Disable upstream initScript which creates /sbin/init as a shell script wrapper.
        # We use direct symlinks instead for simplicity and impermanence compatibility.
        boot.loader.initScript.enable = lib.mkForce false;

        # For nixos-rebuild switch: update /sbin/init to point to the new system's init.
        # The tarball already includes /sbin/init (via contents above), but this is
        # needed when upgrading the system after initial deployment.
        system.build.installBootLoader = pkgs.writeScript "install-lxc-sbin-init.sh" ''
          #!${pkgs.runtimeShell}
          ${pkgs.coreutils}/bin/ln -fs "$1/init" /sbin/init
        '';

        # Create /init symlink during activation (some tools expect this path)
        system.activationScripts.installInitScript = lib.mkForce ''
          ln -fs $systemConfig/init /init
        '';
      };
    };
}
