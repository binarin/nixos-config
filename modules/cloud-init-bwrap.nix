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
# /usr/sbin/ip and `systemctl` at /bin/systemctl (re-adding /bin/sh, since
# --tmpfs /bin hides the host's). Scoped to the four cloud-init systemd units
# only — no global pkgs.cloud-init overlay (avoids rebuilding everything that
# depends on it).
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
      # Everything else is the real host root (ro), with the dirs cloud-init
      # writes bound read-write.
      launcher = pkgs.writeShellScriptBin "cloud-init-bwrap" ''
        exec ${pkgs.bubblewrap}/bin/bwrap \
          --ro-bind / / \
          --dev-bind /dev /dev \
          --proc /proc \
          --tmpfs /bin \
          --tmpfs /usr \
          --symlink ${shBin} /bin/sh \
          --symlink ${systemctlBin} /bin/systemctl \
          --symlink ${ipBin} /usr/sbin/ip \
          --bind /run /run \
          --bind /var/lib/cloud /var/lib/cloud \
          --bind /etc /etc \
          --bind /var/log /var/log \
          -- ${realCloudInit}/bin/cloud-init "$@"
      '';

      # Override ExecStart for each cloud-init unit to use the wrapper, and
      # rewrite `path` so the unwrapped cloud-init isn't pulled into the unit
      # PATH (the wrapper is referenced by absolute path in ExecStart).
      wrapUnit =
        args:
        {
          serviceConfig.ExecStart = lib.mkForce "${lib.getExe launcher} ${args}";
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
        systemd.services = {
          cloud-init-local = wrapUnit "init --local";
          cloud-init = wrapUnit "init";
          cloud-config = wrapUnit "modules --mode=config";
          cloud-final = wrapUnit "modules --mode=final";
        };
      };
    };
}
