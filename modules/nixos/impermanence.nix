{pkgs, lib, config, flake, ...}:
let
  inherit (flake) inputs;
in
{
  imports = [
    inputs.impermanence.nixosModules.impermanence
  ];

  config = lib.mkIf config.hostConfig.feature.impermanence (lib.mkMerge [
    {
      # XXX temp to check what's going on in stage1 systemd
      # boot.initrd.systemd.emergencyAccess = true;

      users.mutableUsers = false;

      # programs.fuse.userAllowOther = true;

      boot.initrd.systemd.services.impermanence-root-rollback = {
        description = "Rollback root btrfs subvolume to a pristine state";
        wantedBy = [
          "initrd.target"
        ];
        after = [
          "dev-main-all.device"
        ];
        before = [
          "sysroot.mount"
        ];
        path = with pkgs; [
          btrfs-progs
          findutils
          # core-utils / util-linux "mount" are already in /bin
        ];
        unitConfig.DefaultDependencies = "no";
        serviceConfig.Type = "oneshot";
        script = ''
        export PATH="$PATH:/bin"
        mkdir /btrfs_tmp
        mount /dev/main/all /btrfs_tmp
        if [[ -e /btrfs_tmp/root ]]; then
            mkdir -p /btrfs_tmp/old_roots
            timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
            mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
        fi

        delete_subvolume_recursively() {
            IFS=$'\n'
            for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
                delete_subvolume_recursively "/btrfs_tmp/$i"
            done
            btrfs subvolume delete "$1"
        }

        for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30); do
            delete_subvolume_recursively "$i"
        done

        btrfs subvolume create /btrfs_tmp/root

        # They are created with 'Q' by tmpfiles, let's prevent creating those subvolumes
        mkdir -p /btrfs_tmp/root/var/lib/{machines,portables}

        umount /btrfs_tmp
      '';
      };

      sops.age.sshKeyPaths = [ "/persist/ssh/ssh_host_ed25519_key" ];

      programs.ssh.extraConfig = ''
        # UserKnownHostsFile /persist/%d/.ssh/known_hosts.d
        IdentityFile /persist/%d/.ssh/keys.d/id_rsa
        IdentityFile /persist/%d/.ssh/keys.d/id_ecdsa
        IdentityFile /persist/%d/.ssh/keys.d/id_ecdsa_sk
        IdentityFile /persist/%d/.ssh/keys.d/id_ed25519
        IdentityFile /persist/%d/.ssh/keys.d/id_ed25519_sk
      '';

      system.activationScripts = let
        userFragment = u: let
          home = config.users.users."${u}".home;
        in ''
        mkdir -p /persist/${home} /local/${home}
        chown ${u}:${u} /persist/${home} /local/${home}

        mkdir -p /persist/${home}/.ssh/{known_hosts.d,keys.d}
        chown ${u}:${u} /persist/${home}/.ssh/{known_hosts.d,keys.d}
      '';
        createPerUserDirs = with lib; concatStringsSep "\n" (map userFragment config.hostConfig.managedUsers);
      in {
        "manual-impermanence-create-dirs" = {
          deps = [ "users" "groups" ];
          text = ''
          mkdir -p /local/etc/NetworkManager/system-connections/
          mkdir -p /local/var/lib/bluetooth/
          mkdir -p /local/var/lib/tailscale/

          mkdir -p /persist/sbctl
          chmod 0700 /persist/sbctl

          ${createPerUserDirs}
        '';
        };
      };

      services.openssh = {
        enable = true;
        hostKeys = [
          {
            path = "/persist/ssh/ssh_host_ed25519_key";
            type = "ed25519";
          }
          {
            path = "/persist/ssh/ssh_host_rsa_key";
            type = "rsa";
            bits = 4096;
          }
        ];
      };

      environment.etc."NetworkManager/system-connections" = {
        source = "/local/etc/NetworkManager/system-connections/";
      };

      systemd.tmpfiles.rules = [
        "L+ /var/lib/bluetooth - - - - /local/var/lib/bluetooth"
        "L+ /var/lib/tailscale - - - - /local/var/lib/tailscale"
      ];

      environment.persistence."/persist" = {
        enable = true;
        hideMounts = true;
        directories = [
          "/var/lib/nixos"
          "/nix/var/nix"
        ];
        files = [
          "/etc/machine-id"
        ];
      };

      environment.persistence."/local" = {
        enable = true;
        hideMounts = true;
        directories = [
          "/var/log"
          "/var/lib/systemd/coredump"
        ];
      };

      programs.sbctl.pkiBundle = "/persist/sbctl";
    }
    (lib.mkIf config.virtualisation.docker.enable {
      environment.persistence."/persist" = {
        directories = [
          "/var/lib/docker"
        ];
      };
    })
  ]);
}
