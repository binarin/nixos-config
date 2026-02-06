{ inputs, ... }:
{
  config = {
    # Add microvm.nix flake input
    flake-file.inputs = {
      microvm.url = "github:astro/microvm.nix";
    };

    flake.nixosModules.microvms =
      { config, lib, ... }:
      {
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

            # Attach VM tap interfaces to bridge
            networks."20-vm-tap" = {
              matchConfig.Name = "vm-*";
              networkConfig.Bridge = config.nixos-config.microvms.bridgeName;
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
  };
}
