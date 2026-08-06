{
  config,
  lib,
  ...
}:
{
  flake.nixosModules.provision-clan-key =
    { pkgs, config, lib, ... }:
    let
      cfg = config.boot.initrd.provisionClanKey;
      # Mirror clan's own key location so we write where clan's Stage 2
      # decryption expects to read.
      secretLocation =
        config.clan.core.vars.age.secretLocation or "/etc/secret-vars";
      # In the initrd the real root is mounted at /sysroot.
      keyPath = "/sysroot${secretLocation}/key.txt";
    in
    {
      key = "nixos-config.modules.nixos.provision-clan-key";

      options.boot.initrd.provisionClanKey = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Fetch the clan age key from a NoCloud cloud-init seed drive
            (e.g. Proxmox CloudInit drive) during Stage 1, after the real
            root is mounted at /sysroot and before pivot.

            The key is written to {option}`clan.core.vars.age.secretLocation`
            + `/key.txt` (default `/etc/secret-vars/key.txt`), only if it
            does not already exist. This lets a freshly-imaged machine
            bootstrap its clan/sops secrets on first boot without needing
            an out-of-band key upload, and without racing Stage 2
            consumers (clan's decrypt units, sops, activation scripts).

            Requires {option}`boot.initrd.systemd.enable`.
          '';
        };

        seedLabel = lib.mkOption {
          type = lib.types.str;
          default = "cidata";
          description = ''
            Filesystem label of the NoCloud seed device, as produced by
            `genisoimage -volid <label>` / Proxmox's CloudInit drive.
          '';
        };

        userDataFile = lib.mkOption {
          type = lib.types.str;
          default = "user-data";
          description = ''
            File name within the seed to extract the age key from. The
            unit greps for a line starting with `AGE-SECRET-KEY-1`, so the
            file may contain arbitrary other cloud-config content.
          '';
        };

        fsTypes = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [
            "iso9660"
            "vfat"
          ];
          description = ''
            Filesystem types to try when mounting the seed, in order.
            Proxmox historically uses iso9660; some setups use FAT.
          '';
        };
      };

      config = lib.mkIf cfg.enable {
        # We need a systemd-managed initrd to get initrd.target / sysroot.mount
        # and a sane unit graph. Stage 1 only.
        boot.initrd.systemd.enable = true;

        assertions = [
          {
            assertion = config.boot.initrd.systemd.enable;
            message = ''
              boot.initrd.provisionClanKey requires boot.initrd.systemd.enable.
              The unit depends on initrd.target / sysroot.mount ordering, which
              only exists with a systemd-managed initrd.
            '';
          }
        ];

        boot.initrd.systemd.storePaths = with pkgs; [
          util-linux # mount, umount, findmnt
          coreutils # mkdir, chmod, grep, etc.
        ];

        boot.initrd.systemd.services.provision-clan-key = {
          description = "Fetch clan age key from NoCloud seed";
          # Run after the real root is mounted, before we pivot.
          wantedBy = [ "initrd.target" ];
          after = [ "sysroot.mount" ];
          before = [ "initrd.target" ];
          # No network needed: NoCloud seed is a local block device.
          unitConfig.DefaultDependencies = "no";
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          # No PATH in initrd units by default; use absolute binaries from storePaths.
          path = with pkgs; [
            util-linux
            coreutils
          ];
          script = ''
            set -x

            key_dest="${keyPath}"

            if [ -s "$key_dest" ]; then
              echo "provision-clan-key: $key_dest already present, skipping"
              exit 0
            fi

            seed_dev="/dev/disk/by-label/${cfg.seedLabel}"
            if [ ! -b "$seed_dev" ]; then
              echo "provision-clan-key: seed device $seed_dev not found"
              echo "Available block devices and labels:"
              ls -l /dev/disk/by-label/ 2>/dev/null || true
              exit 0
            fi

            seed_mnt="$(mktemp -d)"
            mounted=no
            for fst in ${lib.concatMapStringsSep " " (s: s) cfg.fsTypes}; do
              echo "provision-clan-key: trying to mount $seed_dev as $fst"
              if mount -t "$fst" -o ro "$seed_dev" "$seed_mnt"; then
                mounted=yes
                break
              fi
            done

            if [ "$mounted" != yes ]; then
              echo "provision-clan-key: could not mount $seed_dev"
              rmdir "$seed_mnt" 2>/dev/null || true
              # Not fatal: machine may simply have no seed attached.
              exit 0
            fi

            user_data="$seed_mnt/${cfg.userDataFile}"
            echo "provision-clan-key: reading $user_data"

            # An age secret key is a single line beginning with AGE-SECRET-KEY-1.
            # That prefix is globally unique, so a grep anchor is a robust
            # extractor regardless of whatever else is in user-data.
            if key_line="$(grep -m1 '^AGE-SECRET-KEY-1' "$user_data")"; then
              mkdir -p "$(dirname "$key_dest")"
              printf '%s\n' "$key_line" > "$key_dest"
              chmod 600 "$key_dest"
              echo "provision-clan-key: wrote $key_dest"
            else
              echo "provision-clan-key: no AGE-SECRET-KEY-1 line in $user_data"
            fi

            umount "$seed_mnt"
            rmdir "$seed_mnt" 2>/dev/null || true
          '';
        };
      };
    };
}
