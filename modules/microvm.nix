{ self, inputs, ... }:
let
  workspace = "/persist/home/binarin/workspaces";
in
{
  flake-file.inputs.microvm = {
    url = "github:microvm-nix/microvm.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  flake.nixosModules.use-microvms =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.use-microvms";
      imports = [
        inputs.microvm.nixosModules.host
      ];
      config = {
        networking.useNetworkd = true;
        systemd.network = {
          netdevs."10-microvm".netdevConfig = {
            Kind = "bridge";
            Name = "microvm";
          };
          networks."10-microvm" = {
            matchConfig.Name = "microvm";
            networkConfig = {
              ConfigureWithoutCarrier = true;
              DHCP = false;
              DHCPServer = false;
              IPv6AcceptRA = false;
            };
            linkConfig.RequiredForOnline = false;
            addresses = [
              {
                Address = "192.168.83.1/24";
                DuplicateAddressDetection = "none";
              }
            ];
          };
          networks."11-microvm" = {
            matchConfig.Name = "microvm-*";
            networkConfig.Bridge = "microvm";
          };
        };

        services.squid = {
          enable = true;
          proxyAddress = "192.168.83.1";
          extraConfig = ''
            acl microvm src 192.168.83.0/24
            cache deny all
            cache_dir null /tmp
            http_access allow microvm
            http_access deny all
          '';
        };

        networking.firewall.interfaces.microvm.allowedTCPPorts = [ 3128 ];

        microvm.vms = {
          microvm-nixos-config = {
            autostart = false;
            pkgs = import inputs.nixpkgs {
              config = config.nixpkgs.config;
              system = "x86_64-linux";
            };
            specialArgs.inventoryHosName = "microvm-nixos-config";
            config = {
              imports = [
                self.nixosModules.microvm-nixos-config-configuration
              ];
            };
          };
        };
      };
    };

  flake.nixosConfigurations.microvm-nixos-config = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inventoryHostName = "microvm-nixos-config";
    };
    modules = [
      self.nixosModules.microvm-nixos-config-configuration
    ];
  };

  flake.nixosModules.microvm-nixos-config-configuration =
    { lib, config, ... }:
    {
      key = "nixos-config.modules.nixos.microvm-nixos-config-configuration";
      imports = [
        inputs.microvm.nixosModules.microvm
        self.nixosModules.baseline
        {
          nixpkgs.config = lib.mkForce {
          };
          home-manager.users.binarin = {
            imports = [
              self.homeModules.ai-tools
            ];
          };
          microvm.hypervisor = "cloud-hypervisor";
          nixos-config.export-metrics.enable = false;
          system.stateVersion = "25.11";
          networking.proxy.default = "http://192.168.83.1:3128";
          nix.extraOptions = ''
            build-dir = /nix/build
          '';
          microvm = {
            vcpu = 8;
            mem = 8192;
            vsock.cid = 3;
            interfaces = [
              {
                type = "tap";
                id = "microvm-3";
                mac = "02:00:00:00:00:03";
              }
            ];
            writableStoreOverlay = "/nix/.rw-store";
            shares = [
              {
                source = "/nix/store";
                mountPoint = "/nix/.ro-store";
                tag = "ro-store";
                proto = "virtiofs";
              }
              {
                proto = "virtiofs";
                tag = "ssh-keys";
                source = "${workspace}/${config.networking.hostName}/ssh-host-keys";
                mountPoint = "/etc/ssh/host-keys";
              }
              {
                proto = "virtiofs";
                tag = "claude-credentials";
                source = "${workspace}/${config.networking.hostName}/claude-credentials";
                mountPoint = "/home/binarin/.claude";
              }
              {
                proto = "virtiofs";
                tag = "nix-store";
                source = "${workspace}/${config.networking.hostName}/nix-store";
                mountPoint = "/nix/.rw-store";
              }
              {
                proto = "virtiofs";
                tag = "nix-build";
                source = "${workspace}/${config.networking.hostName}/nix-build";
                mountPoint = "/nix/build";
              }
              {
                proto = "virtiofs";
                tag = "workspace";
                source = "/persist/home/binarin/personal-workspace/microvm-workspace/nixos-config";
                mountPoint = "/home/binarin/personal-workspace/microvm-workspace/nixos-config";
              }
            ];
          };
          services.openssh.hostKeys = [
            {
              path = "/etc/ssh/host-keys/ssh_host_ed25519_key";
              type = "ed25519";
            }
          ];
          services.resolved.enable = true;
          networking.useDHCP = false;
          networking.useNetworkd = true;
          networking.tempAddresses = "disabled";
          systemd.network.enable = true;
          systemd.network.networks."10-e" = {
            matchConfig.Name = "e*";
            addresses = [ { Address = "192.168.83.3/24"; } ];
          };
          systemd.settings.Manager.DefaultTimeoutStopSec = "5s";
          # Fix for microvm shutdown hang (issue #170):
          # Without this, systemd tries to unmount /nix/store during shutdown,
          # but umount lives in /nix/store, causing a deadlock.
          systemd.mounts = [
            {
              what = "store";
              where = "/nix/store";
              overrideStrategy = "asDropin";
              unitConfig.DefaultDependencies = false;
            }
          ];
        }
      ];
    };
}
