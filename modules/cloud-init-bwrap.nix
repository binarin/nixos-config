# cloud-init-bwrap.nix
#
# cloud-init's networkd renderer probes for `ip` and `systemctl` via
# `subp.which(prog, search=["/usr/sbin", "/bin"])` — it IGNORES $PATH and
# hardcodes those two directories (cloudinit/net/networkd.py::available). On
# nixos those dirs don't exist, so the renderer writes the .network file but
# the NetworkdActivator reports unavailable, cloud-init writes `.skip-network`,
# and systemd-networkd never reconfigures the interface → no static IP.
#
# Fix: run cloud-init inside a bubblewrap namespace that exposes `ip` at
# /usr/sbin/ip and `systemctl` at /bin/systemctl. The host filesystem stays
# read-write as-is — we're not hardening anything, just injecting the two
# deps cloud-init's hardcoded search expects. Scoped to the four cloud-init
# systemd units only — no global pkgs.cloud-init overlay.
{
  self,
  lib,
  pkgs,
  ...
}:
{
  flake.nixosModules.cloud-init-bwrap =
    { config, lib, pkgs, ... }:
    let
      # The two binaries cloud-init's networkd renderer hardcodes a search for.
      ipBin = "${pkgs.iproute2}/bin/ip";
      systemctlBin = "${config.systemd.package}/bin/systemctl";
      shBin = "${pkgs.bash}/bin/sh";
      realCloudInit = pkgs.cloud-init;

      # Launcher: execs the real cloud-init inside a bwrap namespace where
      # /bin and /usr/sbin contain exactly the binaries cloud-init looks for.
      # Everything else is the real host root (rw); the symlinks layer the
      # binaries on top.
      launcher = pkgs.writeShellScriptBin "cloud-init-bwrap" ''
        exec ${pkgs.bubblewrap}/bin/bwrap \
          --bind / / \
          --dev-bind /dev /dev \
          --proc /proc \
          --symlink ${shBin} /bin/sh \
          --symlink ${systemctlBin} /bin/systemctl \
          --symlink ${ipBin} /usr/sbin/ip \
          -- ${realCloudInit}/bin/cloud-init "$@"
      '';

      # Override ExecStart for each cloud-init unit to use the wrapper, and
      # rewrite `path` so the unwrapped cloud-init isn't pulled into the unit
      # PATH (the wrapper is referenced by absolute path in ExecStart).
      wrapUnit =
        args:
        {
          serviceConfig.ExecStart = lib.mkForce "${lib.getExe launcher} ${args}";
          # cloud-init's stages are boot-time one-shots: re-running them during
          # a nixos-rebuild/deploy switch re-crawls the datasource and re-runs
          # the `always`-frequency modules for no benefit, and it can fail the
          # whole activation. `cloud-init init` loads the existing
          # /var/lib/cloud/data/status.json, *extends* v1.init.errors with the
          # current run's errors, and exits `len(v1[mode]["errors"])` (see
          # cloudinit/cmd/main.py::status_wrapper). Only the `init-local` stage
          # clears that file. So a single module error recorded at boot makes
          # every later restart of cloud-init.service exit non-zero for the
          # rest of the boot, even when the restarted run itself has zero
          # failures — which fails the deploy and triggers an auto-rollback.
          restartIfChanged = false;

          path = lib.mkForce (
            lib.remove pkgs.cloud-init (
              with pkgs;
              [
                iproute2
                net-tools
                openssh
                shadow
                util-linux
                busybox
              ]
              ++ lib.optional config.services.cloud-init.btrfs.enable btrfs-progs
              ++ lib.optional config.services.cloud-init.ext4.enable e2fsprogs
              ++ lib.optional config.services.cloud-init.xfs.enable xfsprogs
              ++ config.services.cloud-init.extraPackages
            )
          );
        };
    in
    {
      config = lib.mkIf config.services.cloud-init.enable {
        # nixos owns user/group management; cloud-init's `users-groups` module
        # runs `passwd`, which can't open nixos's overlay-managed /etc/passwd
        # (`passwd: cannot open /etc/passwd`) and aborts cloud-init.service.
        # Drop it from the module list so the rest of cloud-init runs clean.
        services.cloud-init.settings.cloud_init_modules = lib.mkForce [
          "migrator"
          "seed_random"
          "bootcmd"
          "write-files"
          "growpart"
          "resizefs"
          "update_hostname"
          "resolv_conf"
          "ca-certs"
          "rsyslog"
        ];

        systemd.services = {
          cloud-init-local = wrapUnit "init --local";
          cloud-init = wrapUnit "init";
          cloud-config = wrapUnit "modules --mode=config";
          cloud-final = wrapUnit "modules --mode=final";
        };
      };
    };
}
