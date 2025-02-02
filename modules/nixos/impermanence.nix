{pkgs, lib, config, flake, ...}:
let
  inherit (flake) inputs;
in
{
  # nixos-install --root /mnt/ --option experimental-features "nix-command flakes" --option builders "ssh://nix-build x86_64-linux" --flake /run/media/nixos/orico-nvme-ext4/demandred-old-persist/binarin-shelter/personal-workspace/nixos-config#demandred
  imports = [
    inputs.impermanence.nixosModules.impermanence
  ];
  config = lib.mkIf config.hostConfig.feature.impermanence {

    users.mutableUsers = false;

    programs.fuse.userAllowOther = true;

    # boot.initrd.postResumeCommands = lib.mkAfter ''
    #   mkdir /btrfs_tmp
    #   mount /dev/main/all /btrfs_tmp
    #   if [[ -e /btrfs_tmp/root ]]; then
    #       mkdir -p /btrfs_tmp/old_roots
    #       timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
    #       mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
    #   fi

    #   delete_subvolume_recursively() {
    #       IFS=$'\n'
    #       for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
    #           delete_subvolume_recursively "/btrfs_tmp/$i"
    #       done
    #       btrfs subvolume delete "$1"
    #   }

    #   for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30); do
    #       delete_subvolume_recursively "$i"
    #   done

    #   btrfs subvolume create /btrfs_tmp/root
    #   umount /btrfs_tmp
    # '';

    # boot.initrd.systemd.services.rollback = {
    #   description = "Rollback root btrfs subvolume to a pristine state";
    #   wantedBy = [
    #     "initrd.target"
    #   ];
    #   after = [
    #     "zfs-import-rpool.service"
    #   ];
    #   before = [
    #     "sysroot.mount"
    #   ];
    #   path = with pkgs; [
    #     zfs
    #   ];
    #   unitConfig.DefaultDependencies = "no";
    #   serviceConfig.Type = "oneshot";
    #   script = ''
    #     zfs rollback -r rpool/local/root@blank && echo "rollback complete"
    #   '';
    # };

    system.activationScripts = let
      userFragment = u: let
        home = config.users.users."${u}".home;
      in ''
        mkdir -p /persist/${home} /local/${home}
        chown ${u}:${u} /persist/${home} /local/${home}
      '';
      createPerUserDirs = with lib; concatStringsSep "\n" (map userFragment config.hostConfig.managedUsers);
    in {
      "manual-impermanence-create-dirs" = {
        deps = [ "users" "groups" ];
        text = ''
          mkdir -p /local/etc/NetworkManager/system-connections/
          mkdir -p /local/var/lib/bluetooth/

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
      "L /var/lib/bluetooth - - - - /local/var/lib/bluetooth"
    ];

    environment.persistence."/persist" = {
      enable = true;
      hideMounts = true;
      directories = [
        "/var/lib/nixos"
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
  };
}
