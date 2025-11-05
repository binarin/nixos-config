{ self, inputs, config, lib, ... }: let
install = ''
nix run github:nix-community/nixos-anywhere -- \
 --generate-hardware-config nixos-generate-config machines/qdevice/hardware-configuration.nix \
 --flake "$(pwd)#qdevice" \
 --target-host root@192.168.2.16 \
 --disk-encryption-keys /tmp/password-without-newline /tmp/password-without-newline
'';
in {
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
    ];
  };

  flake.nixosModules.qdevice-configuration = {lib, config, pkgs, inventoryHostname, ...}: {
    key = "nixos-config.qdevice-configuration";
    imports = [
      self.nixosModules.default-new
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
      systemd.network.enable = true;

      networking.hostName = inventoryHostname;

      systemd.network.networks."40-enp2s0" = {
        matchConfig.Name = "enp2s0";
        dns = [ "192.168.2.1" ];
        address = [ "192.168.2.16/24" ];
        routes = [ { Gateway = "192.168.2.1"; } ];
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

      services.tang = {
        enable = true;
        listenStream = [ "192.168.2.16:7654" ];
        ipAddressAllow = [ "192.168.2.0/24" ];
      };
    };
  };
}
