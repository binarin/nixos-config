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

        systemd.network.networks."40-lxc" = {
          matchConfig.Name = "eth0";
          dns = flakeConfig.inventory.networks.home.dns;
          address = [
            flakeConfig.inventory.ipAllocation."${config.networking.hostName}".home.primary.addressWithPrefix
          ];
          routes = [ { Gateway = flakeConfig.inventory.networks.home.gateway; } ];
        };

        services.getty.autologinUser = "root";

        # XXX this creates /sbin/init with /bin/sh shebang, not compatible with impermanence
        boot.loader.initScript.enable = lib.mkForce false;
        # XXX so copy it from regular lxc-container.nix for the time being
        system.build.installBootLoader = pkgs.writeScript "install-lxc-sbin-init.sh" ''
          #!${pkgs.runtimeShell}
          ${pkgs.coreutils}/bin/ln -fs "$1/init" /sbin/init
        '';
        system.activationScripts.installInitScript = lib.mkForce ''
          ln -fs $systemConfig/init /init
        '';
      };
    };
}
