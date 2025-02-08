{ flake, config, lib, pkgs, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  create = lxcId: ''
    pct create ${lxcId} /var/lib/vz/template/cache/proxmox-lxc-docker-on-nixos.tar.xz \
      --cores 4 --features nesting=1 --memory 8192 --swap 0 \
      --unprivileged 1 --ostype unmanaged --arch amd64 \
      --rootfs local-zfs:8 \
      --mp0 local-zfs:1,mp=/bin,backup=1 \
      --mp0 local-zfs:1,mp=/sbin,backup=1 \
      --mp1 local-zfs:8,mp=/nix,backup=1 \
      --mp2 local-zfs:8,mp=/persist,backup=1 \
      --mp3 local-zfs:1,mp=/local,backup=0 \
      --mp3 local-zfs:1,mp=/boot,backup=1 \
      --net0 name=eth0,bridge=vmbr0,firewall=1,ip=dhcp,type=veth

    cat <<EOF >> /etc/pve/lxc/${lxcId}.conf
    lxc.cgroup2.devices.allow: c 10:200 rwm
    lxc.mount.entry: /dev/net/tun dev/net/tun none bind,create=file
    EOF

    ROOTFS=rpool/data/subvol-${lxcId}-disk-0
    zfs destroy $ROOTFS
    zfs create $ROOTFS
    chown -v 100000:100000 /$ROOTFS
    zfs snapshot rpool/data/subvol-${lxcId}-disk-0@blank

    touch /$ROOTFS/nix-path-registration
    chown -Rv 100000:100000 /$ROOTFS

    pct start ${lxcId}
  '';

in {
  lib.lxc.createCommand = create;
  virtualisation.docker.enable = true;
  virtualisation.docker.autoPrune.enable = true;
  virtualisation.arion.backend = "docker";

  # XXX this creates /sbin/init with /bin/sh shebang, not compatible with impermanence
  boot.loader.initScript.enable = lib.mkForce false;

  # XXX so copy it from regular lxc-container.nix for the time being
  system.build.installBootLoader = pkgs.writeScript "install-lxc-sbin-init.sh" ''
    #!${pkgs.runtimeShell}
    ${pkgs.coreutils}/bin/ln -fs "$1/init" /sbin/init
  '';
  system.activationScripts.installInitScript = lib.mkForce ''
    ln -fs $systemConfig/init /init
  '';

  # sops.secrets.tailscale-auth = { };
  # services.tailscale = {
  #   enable = true;
  #   authKeyFile = config.sops.secrets.tailscale-auth.path;
  # };
}
