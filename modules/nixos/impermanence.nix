{pkgs, lib, config, flake, ...}:
let
  inherit (flake) inputs;
in
{
  imports = [
    inputs.impermanence.nixosModules.impermanence
  ];
  config = lib.mkIf config.hostConfig.feature.impermanence {

    fileSystems."/" =
      { device = "rpool/local/root";
        fsType = "zfs";
      };

    fileSystems."/nix" =
      { device = "rpool/local/nix";
        fsType = "zfs";
      };

    fileSystems."/persist" =
      { device = "rpool/safe/persist";
        fsType = "zfs";
        neededForBoot = true;
      };

    fileSystems."/cache" =
      { device = "rpool/local/cache";
        fsType = "zfs";
        neededForBoot = true;
      };

    users.mutableUsers = false;

    programs.fuse.userAllowOther = true;

    boot.initrd.systemd.services.rollback = {
      description = "Rollback ZFS datasets to a pristine state";
      wantedBy = [
        "initrd.target"
      ];
      after = [
        "zfs-import-rpool.service"
      ];
      before = [
        "sysroot.mount"
      ];
      path = with pkgs; [
        zfs
      ];
      unitConfig.DefaultDependencies = "no";
      serviceConfig.Type = "oneshot";
      script = ''
        zfs rollback -r rpool/local/root@blank && echo "rollback complete"
      '';
    };

    system.activationScripts = let
      userFragment = u: let
        home = config.users.users."${u}".home;
      in ''
        mkdir -p /persist/${home} /cache/${home}
        chown ${u}:${u} /persist/${home} /cache/${home}
      '';
      createPerUserDirs = with lib; concatStringsSep "\n" (map userFragment config.hostConfig.managedUsers);
    in {
      "manual-impermanence-create-dirs" = {
        deps = [ "users" "groups" ];
        text = ''
          mkdir -p /cache/etc/NetworkManager/system-connections/
          mkdir -p /cache/var/lib/bluetooth/

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
      source = "/cache/etc/NetworkManager/system-connections/";
    };

    systemd.tmpfiles.rules = [
      "L /var/lib/bluetooth - - - - /cache/var/lib/bluetooth"
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

    environment.persistence."/cache" = {
      enable = true;
      hideMounts = true;
      directories = [
        "/var/log"
        "/var/lib/systemd/coredump"
      ];
    };
  };
}
