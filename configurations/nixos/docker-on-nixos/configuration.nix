{
  flake,
  config,
  lib,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  create = lxcId: ''
    pct create ${lxcId} /var/lib/vz/template/cache/proxmox-lxc-docker-on-nixos.tar.xz \
      --hostname docker-on-nixos \
      --onboot 1 \
      --cores 4 --features nesting=1 --memory 8192 --swap 0 \
      --unprivileged 1 --ostype unmanaged --arch amd64 \
      --rootfs local-zfs:8 \
      --mp0 local-zfs:1,mp=/sbin,backup=1 \
      --mp1 local-zfs:8,mp=/nix,backup=1 \
      --mp2 local-zfs:8,mp=/persist,backup=1 \
      --mp3 local-zfs:1,mp=/local,backup=0 \
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

    cat <<"EOF" | tee /var/lib/vz/snippets/pve-impermanence-hook.pl > /dev/null
    ${config.lib.self.read "pve-impermanence-hook.pl"}
    EOF
    chmod +x /var/lib/vz/snippets/pve-impermanence-hook.pl
    pct set 116 --hookscript local:snippets/pve-impermanence-hook.pl

    pct start ${lxcId}
  '';
in
{
  imports = [
    ./brick-tracker.nix
    ./homebox.nix
  ];

  lib.lxc.createCommand = create;

  virtualisation.docker.enable = true;
  virtualisation.docker.autoPrune.enable = true;
  virtualisation.arion.backend = "docker";

  environment.persistence."/persist".directories = [
    "/var/lib/docker"
  ];

  services.caddy.expose-local-http.enable = true;
}
