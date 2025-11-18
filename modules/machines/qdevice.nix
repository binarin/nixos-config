{
  self,
  inputs,
  lib,
  config,
  ...
}:

{
  flake.deploy.nodes.qdevice = {
    hostname = config.inventory.ipAllocation."qdevice".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.qdevice;
    };
  };

  flake.nixosConfigurations.qdevice = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inventoryHostName = "qdevice";
    };
    modules = [
      self.nixosModules.qdevice-configuration
    ];
  };

  flake.nixosModules.qdevice-configuration =
    {
      lib,
      config,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.qdevice-configuration";

      imports = [
        self.nixosModules.default
        self.nixosModules.systemd-boot
        self.nixosModules.disko
        inputs.arion.nixosModules.arion
        "${self}/machines/qdevice/hardware-configuration.nix"
      ];

      config = {
        networking.hostName = "qdevice";
        system.stateVersion = "25.05";

        boot.initrd.clevis.enable = true;
        boot.initrd.clevis.useTang = true;
        boot.initrd.clevis.devices."luks1".secretFile = "${self}/secrets/qdevice/luks.jwe";

        boot.initrd.availableKernelModules = [ "igc" ]; # network card, to be able to

        boot.initrd.systemd.enable = true;
        boot.initrd.systemd.network.enable = true;

        users.users.root.openssh.authorizedPrincipals = [ "qdevice" ];
        users.users.root.openssh.authorizedKeys.keys = config.lib.publicKeys.secureWithTag "presence";

        networking.useDHCP = false;

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
              dns = config.inventory.networks.home.dns;
              address = [
                config.inventory.ipAllocation."${config.networking.hostName}".home.primary.addressWithPrefix
              ];
              routes = [ { Gateway = config.inventory.networks.home.gateway; } ];
              bridgeConfig = { };
              linkConfig = {
                RequiredForOnline = "routable";
              };
            };
          };
        };

        boot.initrd.systemd.network.networks."40-enp2s0" = {
          matchConfig.Name = "enp2s0";
          dns = config.inventory.networks.home.dns;
          address = [
            config.inventory.ipAllocation."${config.networking.hostName}".home.primary.addressWithPrefix
          ];
          routes = [ { Gateway = config.inventory.networks.home.gateway; } ];
        };

        networking.firewall.enable = true;

        networking.firewall.allowedTCPPorts = [
          7654
          1688
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
          listenStream = [
            "${config.inventory.ipAllocation."${config.networking.hostName}".home.primary.address}:7654"
          ];
          ipAddressAllow = [
            "${config.inventory.networks.home.network}/${toString config.inventory.networks.home.prefix}"
          ];
        };

        virtualisation.docker.enable = true;
        virtualisation.docker.autoPrune.enable = true;
        virtualisation.arion.backend = "docker";

        virtualisation.arion.projects.vlmcsd = {
          serviceName = "vlmcsd-docker-compose";
          settings.services.vlmcsd = {
            service = {
              image = "mikolatero/vlmcsd";
              container_name = "vlmcsd";
              ports = [
                "1688:1688"
              ];
              restart = "unless-stopped";
            };
          };
        };
      };
    };
}
