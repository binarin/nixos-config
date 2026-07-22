{
  self,
  inputs,
  config,
  lib,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
in
{
  flake.deploy.nodes.qdevice = {
    hostname = config.inventory.ipAllocation."qdevice".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.qdevice;
    };
  };

  clan.inventory.machines.qdevice = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.qdevice.home.primary.address;
  };

  clan.machines.qdevice = {
    imports = [
      self.nixosModules.qdevice-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosConfigurations.qdevice = lib.mkForce (
    self.clan.nixosConfigurations.qdevice.extendModules {
      specialArgs.inventoryHostName = "qdevice";
    }
  );

  flake.nixosModules.qdevice-configuration =
    {
      config,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.qdevice-configuration";

      imports = [
        self.nixosModules.baseline
        self.nixosModules.systemd-boot
        inputs.disko.nixosModules.disko
        inputs.arion.nixosModules.arion
        "${self}/my-machines/qdevice/disko.nix"
        "${self}/my-machines/qdevice/hardware-configuration.nix"
        self.nixosModules.qdevice-vm
      ];

      config = {
        networking.hostName = "qdevice";

        # Plaintext root password for the qdevice-vm Debian guest. Fed into
        # cloud-init's `chpasswd` by the qdevice-vm module so the serial
        # console / password SSH is usable. Auto-generated once, then stored.
        clan.core.vars.generators.qdevice-vm-root-password = {
          files.root-password.secret = true;
          runtimeInputs = [ pkgs.xkcdpass ];
          script = ''
            xkcdpass --numwords 4 --delimiter - --count 1 | tr -d '\n' > $out/root-password
          '';
        };
        qdevice-vm.rootPasswordFile =
          config.clan.core.vars.generators.qdevice-vm-root-password.files.root-password.path;

        system.stateVersion = "25.05";

        nixos-config.export-metrics.enable = true;

        boot.initrd.clevis.enable = true;
        boot.initrd.clevis.useTang = true;
        boot.initrd.clevis.devices."luks1".secretFile = selfLib.file' "secrets/qdevice/luks.jwe";

        boot.initrd.availableKernelModules = [ "igc" ]; # network card, to be able to

        boot.initrd.systemd.enable = true;
        boot.initrd.systemd.network.enable = true;

        users.users.root.openssh.authorizedPrincipals = [ "qdevice" ];
        users.users.root.openssh.authorizedKeys.keys = config.lib.publicKeys.secureWithTag "presence";

        networking.useDHCP = false;

        # Tailscale is pulled in by baseline but isn't wanted on qdevice.
        services.tailscale.enable = lib.mkForce false;

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
          qemu.runAsRoot = false;
          qemu.swtpm.enable = true;
          onBoot = "ignore"; # autostarded guests are started anyway
          onShutdown = "shutdown";
        };

        environment.systemPackages = with pkgs; [
          virt-manager
          smartmontools
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
          settings.services =
            let
              tags = builtins.fromJSON (builtins.readFile ./vlmcsd.json);
            in
            {
              vlmcsd = {
                service = {
                  image = "mikolatero/vlmcsd:${tags.vlmcsd}";
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
    };
}
