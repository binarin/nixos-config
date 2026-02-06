{ self, inputs, lib, ... }: {
  config = {
    # Add microvm.nix flake input
    flake-file.inputs = {
      microvm.url = "github:astro/microvm.nix";
    };

    flake.nixosModules.microvms = { config, lib, pkgs, ... }: {
      key = "nixos-config.modules.nixos.microvms";

      imports = [
        inputs.microvm.nixosModules.host
      ];

      options = {
        nixos-config.microvms = {
          enable = lib.mkEnableOption "MicroVM setup for coding agents";

          hostInterface = lib.mkOption {
            type = lib.types.str;
            default = "wlp0s20f3";
            description = "Host network interface to use for NAT";
          };

          bridgeName = lib.mkOption {
            type = lib.types.str;
            default = "microbr";
            description = "Bridge interface name";
          };

          subnet = lib.mkOption {
            type = lib.types.str;
            default = "192.168.83";
            description = "Subnet for microVMs (without /24)";
          };

          credentialsDir = lib.mkOption {
            type = lib.types.str;
            default = "/persist/home/binarin/claude-microvm";
            description = "Directory for Claude credentials";
          };

          workspaceBaseDir = lib.mkOption {
            type = lib.types.str;
            default = "/persist/home/binarin/workspaces";
            description = "Base directory for VM workspaces";
          };
        };
      };

      config = lib.mkIf config.nixos-config.microvms.enable {
        # Enable virtualization
        microvm.host.enable = true;

        # Bridge network setup
        systemd.network = {
          enable = true;
          netdevs."10-${config.nixos-config.microvms.bridgeName}" = {
            netdevConfig = {
              Kind = "bridge";
              Name = config.nixos-config.microvms.bridgeName;
            };
          };

          networks."10-${config.nixos-config.microvms.bridgeName}" = {
            matchConfig.Name = config.nixos-config.microvms.bridgeName;
            networkConfig = {
              Address = "${config.nixos-config.microvms.subnet}.1/24";
              DHCPServer = false;
            };
            linkConfig.RequiredForOnline = false;
          };
        };

        # NAT setup
        networking.nat = {
          enable = true;
          externalInterface = config.nixos-config.microvms.hostInterface;
          internalInterfaces = [ config.nixos-config.microvms.bridgeName ];
        };

        # Allow IP forwarding
        boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

        # DNS resolution
        services.resolved.enable = true;

        # Disable firewall for NAT isolation
        networking.firewall.enable = lib.mkDefault false;

        # Ensure credentials and workspace directories exist
        systemd.tmpfiles.settings."10-microvms" = {
          "${config.nixos-config.microvms.credentialsDir}".d = {
            user = "binarin";
            group = "binarin";
            mode = "0700";
          };
          "${config.nixos-config.microvms.workspaceBaseDir}".d = {
            user = "binarin";
            group = "binarin";
            mode = "0755";
          };
        };
      };
    };

    # Base microVM configuration template
    flake.nixosModules.microvm-base = { config, lib, pkgs, ... }: {
      key = "nixos-config.modules.nixos.microvm-base";

      options = {
        microvm.workspace = lib.mkOption {
          type = lib.types.str;
          description = "Path to workspace directory on host";
        };

        microvm.credentialsDir = lib.mkOption {
          type = lib.types.str;
          description = "Path to credentials directory on host";
        };
      };

      config = {
        # Use cloud-hypervisor
        microvm.hypervisor = "cloud-hypervisor";

        # Resource allocation
        microvm.vcpu = 8;
        microvm.mem = 4096;
        microvm.diskSize = 8 * 1024;

        # Shares using virtiofs
        microvm.shares = [
          # Read-only Nix store
          {
            proto = "virtiofs";
            tag = "ro-store";
            source = "/nix/store";
            mountPoint = "/nix/.ro-store";
          }
          # SSH host keys
          {
            proto = "virtiofs";
            tag = "ssh-keys";
            source = "${config.microvm.workspace}/ssh-host-keys";
            mountPoint = "/etc/ssh/host-keys";
          }
          # Claude credentials
          {
            proto = "virtiofs";
            tag = "credentials";
            source = config.microvm.credentialsDir;
            mountPoint = "/home/binarin/claude-microvm";
          }
          # Workspace
          {
            proto = "virtiofs";
            tag = "workspace";
            source = config.microvm.workspace;
            mountPoint = config.microvm.workspace;
          }
        ];

        # Writable Nix store overlay
        boot.initrd.availableKernelModules = [ "overlay" ];
        fileSystems."/nix/.rw-store" = {
          fsType = "tmpfs";
          options = [ "mode=0755" ];
          neededForBoot = true;
        };
        fileSystems."/nix/store" = {
          fsType = "overlay";
          device = "overlay";
          options = [
            "lowerdir=/nix/.ro-store"
            "upperdir=/nix/.rw-store/store"
            "workdir=/nix/.rw-store/work"
          ];
          depends = [
            "/nix/.ro-store"
            "/nix/.rw-store"
          ];
        };

        # Prevent systemd deadlock during shutdown
        systemd.services.nix-store-unmount-workaround = {
          description = "Prevent systemd deadlock on shutdown";
          wantedBy = [ "shutdown.target" ];
          before = [ "shutdown.target" ];
          script = ''
            # Workaround for systemd trying to unmount /nix/store
            # while umount binary lives there
            :
          '';
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
        };

        # Basic system configuration
        system.stateVersion = "25.11";

        # Enable SSH
        services.openssh = {
          enable = true;
          settings.PermitRootLogin = "yes";
        };

        # User configuration
        users.users.binarin = {
          isNormalUser = true;
          uid = 1000;
          extraGroups = [ "wheel" ];
          openssh.authorizedKeys.keyFiles = [ "${config.microvm.workspace}/ssh-host-keys/authorized_keys" ];
        };

        # Allow passwordless sudo for wheel group
        security.sudo.wheelNeedsPassword = false;

        # Minimal packages
        environment.systemPackages = with pkgs; [
          git
          vim
        ];

        # Networking
        systemd.network.enable = true;
      };
    };
  };
}
