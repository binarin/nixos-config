{
  self,
  inputs,
  config,
  lib,
  ...
}:
{
  flake.deploy.nodes.qdevice = {
    hostname = "192.168.2.16";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.qdevice;
    };
  };

  flake.nixosConfigurations.qdevice = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      flake = {
        inherit self inputs config;
      };
      inventoryHostname = "qdevice";
    };

    modules = [
      self.nixosModules.qdevice-configuration
    ]
    ++ self.nixosSharedModules;
  };

  flake.nixosModules.qdevice-configuration =
    {
      lib,
      config,
      pkgs,
      inventoryHostname,
      ...
    }:
    {
      key = "nixos-config.qdevice-configuration";
      imports = [
        self.nixosModules.default
        self.nixosModules.disko
        self.nixosModules.systemd-boot
        inputs.arion.nixosModules.arion
        "${self}/machines/qdevice/hardware-configuration.nix"
      ];

      config = {
        system.stateVersion = "25.05";
        networking.hostId = (import "${self}/inventory/host-id.nix").qdevice;

        boot.initrd.clevis.enable = true;
        boot.initrd.clevis.useTang = true;
        boot.initrd.clevis.devices."luks1".secretFile = "${self}/secrets/qdevice/luks.jwe";

        boot.initrd.availableKernelModules = [ "igc" ]; # network card, to be able to

        boot.initrd.systemd.enable = true;
        boot.initrd.systemd.network.enable = true;

        users.users.root.openssh.authorizedPrincipals = [ "qdevice" ];
        users.users.root.openssh.authorizedKeys.keys = config.lib.publicKeys.secureWithTag "presence";

        networking.useDHCP = false;
        networking.hostName = inventoryHostname;

        systemd.network = {
          enable = true;
          netdevs = {
            "20-br0" = {
              netdevConfig = {
                Kind = "bridge";
                Name = "br0";
              };
            };
          };

          networks = {
            "30-enp2s0" = {
              matchConfig.Name = "enp2s0";
              networkConfig.Bridge = "br0";
              linkConfig.RequiredForOnline = "enslaved";
            };

            "40-br0" = {
              matchConfig.Name = "br0";
              dns = [ "192.168.2.1" ];
              address = [ "192.168.2.16/24" ];
              routes = [ { Gateway = "192.168.2.1"; } ];
              bridgeConfig = { };
              linkConfig = {
                RequiredForOnline = "routable";
              };
            };
          };
        };

        boot.initrd.systemd.network.networks."40-enp2s0" = {
          matchConfig.Name = "enp2s0";
          dns = [ "192.168.2.1" ];
          address = [ "192.168.2.16/24" ];
          routes = [ { Gateway = "192.168.2.1"; } ];
        };

        networking.firewall.enable = true;

        networking.firewall.allowedTCPPorts = [
          7654
        ];

        virtualisation.libvirtd = {
          enable = true;
          qemu.ovmf.enable = true;
          qemu.ovmf.packages = [ pkgs.OVMFFull.fd ];
          qemu.runAsRoot = false;
          qemu.swtpm.enable = true;
          onBoot = "ignore"; # autostarded guests are started anyway
          onShutdown = "shutdown";
        };

        environment.systemPackages = with pkgs; [
          virt-manager
        ];

        services.tang = {
          enable = true;
          listenStream = [ "192.168.2.16:7654" ];
          ipAddressAllow = [ "192.168.2.0/24" ];
        };
      };
    };
}
