{ ... }:
{
  config = {
    flake.nixosModules.microvm-vms =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        cfg = config.nixos-config.microvms;

        # Helper function to create a microVM configuration
        mkMicroVM =
          {
            name,
            ip,
            macAddress,
            workspace,
            cid ? 2,
            extraPackages ? [ ],
            extraConfig ? { },
          }:
          {
            # Auto-start VM
            autostart = true;

            # Fully-declarative VM configuration
            config = lib.recursiveUpdate {
              # Hostname
              networking.hostName = name;

              # Import base NixOS configuration
              imports = [
                # Basic NixOS setup
                (
                  { lib, pkgs, ... }:
                  {
                    # Use cloud-hypervisor
                    microvm.hypervisor = "cloud-hypervisor";

                    # Enable vsock for systemd-notify
                    microvm.vsock.cid = cid;

                    # Resource allocation
                    microvm.vcpu = 8;
                    microvm.mem = 4096;

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
                        source = "${workspace}/ssh-host-keys";
                        mountPoint = "/etc/ssh/host-keys";
                      }
                      # Claude credentials
                      {
                        proto = "virtiofs";
                        tag = "credentials";
                        source = cfg.credentialsDir;
                        mountPoint = "/home/binarin/claude-microvm";
                      }
                      # Workspace
                      {
                        proto = "virtiofs";
                        tag = "workspace";
                        source = workspace;
                        mountPoint = workspace;
                      }
                    ];

                    # Writable Nix store overlay
                    boot.initrd.availableKernelModules = [ "overlay" ];
                    fileSystems."/nix/.rw-store" = {
                      fsType = "tmpfs";
                      options = [ "mode=0755" ];
                      neededForBoot = true;
                    };
                    fileSystems."/nix/store" = lib.mkForce {
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

                    # Network configuration
                    microvm.interfaces = [
                      {
                        type = "tap";
                        id = "vm-${name}";
                        mac = macAddress;
                      }
                    ];

                    systemd.network.enable = true;
                    systemd.network.networks."10-lan" = {
                      matchConfig.Name = "enp*";
                      networkConfig = {
                        Address = "${ip}/24";
                        Gateway = "${cfg.subnet}.1";
                        DNS = "${cfg.subnet}.1";
                      };
                      linkConfig.RequiredForOnline = "no";
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
                      # SSH keys will be mounted from workspace
                      openssh.authorizedKeys.keyFiles = lib.optional (builtins.pathExists "${workspace}/ssh-host-keys/authorized_keys") "${workspace}/ssh-host-keys/authorized_keys";
                    };

                    # Allow passwordless sudo for wheel group
                    security.sudo.wheelNeedsPassword = false;

                    # Minimal packages
                    environment.systemPackages =
                      with pkgs;
                      [
                        git
                        vim
                      ]
                      ++ extraPackages;
                  }
                )
              ];
            } extraConfig;
          };
      in
      {
        key = "nixos-config.modules.nixos.microvm-vms";

        config = lib.mkIf cfg.enable {
          # Example VM: claudevm
          microvm.vms.claudevm = mkMicroVM {
            name = "claudevm";
            ip = "${cfg.subnet}.2";
            macAddress = "02:00:00:00:00:02";
            workspace = "${cfg.workspaceBaseDir}/claudevm";
            extraPackages = with pkgs; [
              nodejs
              python3
            ];
          };
        };
      };
  };
}
