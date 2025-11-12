{ self, ... }:
{
  flake.nixosModules.initrd-ssh =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.initrd-ssh";
      boot.initrd.systemd.contents."/etc/ssh/ssh_host_ed25519_key.jwe".source =
        "${self}/secrets/qdevice/ssh_host_ed25519_key.jwe";
      boot.initrd.systemd.contents."/etc/ssh/ssh_host_rsa_key.jwe".source =
        "${self}/secrets/qdevice/ssh_host_rsa_key.jwe";
      boot.initrd.systemd.contents."/etc/ssh/trusted_user_ca_keys".text = lib.concatStringsSep "\n" (
        config.lib.publicKeys.secureWithTag "user-ca"
      );

      boot.initrd.systemd.enable = true;
      boot.initrd.systemd.network.enable = true;
      # boot.initrd.systemd.emergencyAccess = true;
      # boot.initrd.systemd.initrdBin = with pkgs; [ iproute2 gnugrep procps gnutar ];

      # boot.kernelParams = [
      #   "ip=192.168.2.16::192.168.2.1:255.255.255.0::enp2s0:off"
      #   "nameserver=192.168.2.1"
      # ];

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

      boot.initrd.network.ssh.extraConfig = ''
        Hostkey /etc/ssh/ssh_host_rsa_key
        Hostkey /etc/ssh/ssh_host_ed25519_key
        TrustedUserCaKeys /etc/ssh/trusted_user_ca_keys
      '';

      boot.initrd.network.ssh.enable = true;
      boot.initrd.network.ssh.ignoreEmptyHostKeys = true;
    };
}
