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
      users.users.root.openssh.authorizedPrincipals = lib.mkForce [ "qdevice" ];
      networking.hostId = (import "${self}/inventory/host-id.nix").qdevice;

      boot.initrd.clevis.enable = true;
      boot.initrd.clevis.useTang = true;

      boot.initrd.clevis.devices."luks1".secretFile = "${self}/secrets/qdevice/luks.jwe";

      boot.initrd.availableKernelModules = [ "igc" ]; # network card

      boot.initrd.systemd.contents."/etc/ssh/ssh_host_ed25519_key.jwe".source = "${self}/secrets/qdevice/ssh_host_ed25519_key.jwe";
      boot.initrd.systemd.contents."/etc/ssh/ssh_host_rsa_key.jwe".source = "${self}/secrets/qdevice/ssh_host_rsa_key.jwe";
      boot.initrd.systemd.contents."/etc/ssh/trusted_user_ca_keys".text = lib.concatStringsSep "\n" ( config.lib.publicKeys.secureWithTag "user-ca");

      boot.initrd.systemd.services.initrd-decrypt-ssh-host-keys = {
          description = "Decrypts SSH host keys to use in initrd";
          requiredBy = [ "sshd.service" ];
          requires = [
            "dev-tpmrm0.device"
          ];
          after = [
            "dev-tpmrm0.device"
          ];
          before = [
            "sshd.service"
            "shutdown.target"
          ];
          conflicts = [ "shutdown.target" ];
          unitConfig.DefaultDependencies = false;

          script = ''
            set -x
            clevis decrypt < /etc/ssh/ssh_host_rsa_key.jwe > /etc/ssh/ssh_host_rsa_key || true
            # clevis returns non-zero even on success
            if [[ ! -s /etc/ssh/ssh_host_rsa_key ]]; then
              echo "Failed to decrypt /etc/ssh/ssh_host_rsa_key.jwe"
              exit 1
            fi
            chmod 0600 /etc/ssh/ssh_host_rsa_key

            clevis decrypt < /etc/ssh/ssh_host_ed25519_key.jwe > /etc/ssh/ssh_host_ed25519_key || true
            # clevis returns non-zero even on success
            if [[ ! -s /etc/ssh/ssh_host_ed25519_key ]]; then
              echo "Failed to decrypt /etc/ssh/ssh_host_ed25519_key.jwe"
              exit 1
            fi
            chmod 0600 /etc/ssh/ssh_host_ed25519_key
          '';

          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
        };

      boot.initrd.network.ssh.enable = true;
      boot.initrd.network.ssh.ignoreEmptyHostKeys = true;
      boot.initrd.network.ssh.extraConfig = ''
        Hostkey /etc/ssh/ssh_host_rsa_key
        Hostkey /etc/ssh/ssh_host_ed25519_key
        TrustedUserCaKeys /etc/ssh/trusted_user_ca_keys
      '';

      boot.initrd.systemd.enable = true;
      boot.initrd.systemd.network.enable = true;
      boot.initrd.systemd.emergencyAccess = true;
      boot.initrd.systemd.initrdBin = with pkgs; [ iproute2 gnugrep procps gnutar ];

      # boot.kernelParams = [
      #   "ip=192.168.2.16::192.168.2.1:255.255.255.0::enp2s0:off"
      #   "nameserver=192.168.2.1"
      # ];

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

      # virtualisation.docker.enable = true;
      # virtualisation.arion.backend = "docker";

      # virtualisation.arion.projects.qdevice = {
      #   serviceName = "docker-qdevice";
      #   settings.services.qdevice.service = {
      #     restart = "unless-stopped";
      #     expose = [ "5403" ];
      #     # volumes = [
      #     #   "/var/lib/corosync-qnetd/etc:/etc/corosync/qnetd"
      #     # ];
      #     environment.NSS_IGNORE_SYSTEM_POLICY = "1";
      #     build.context = builtins.toString (pkgs.writeTextFile {
      #       name = "qdevice-Dockerfile";
      #       destination = "/Dockerfile";
      #       text = ''
      #         FROM debian
      #         RUN apt update && apt install -y corosync-qnetd
      #         CMD /usr/bin/corosync-qnetd -f
      #       '';
      #     });
      #   };
      # };
    };
  };
}
