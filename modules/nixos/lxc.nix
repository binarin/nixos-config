{
  flake,
  modulesPath,
  config,
  lib,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  # Need to add to /etc/pve/lxc/XXX.conf
  # lxc.cgroup2.devices.allow: c 10:200 rwm
  # lxc.mount.entry: /dev/net/tun dev/net/tun none bind,create=file
  imports = [
    "${modulesPath}/virtualisation/proxmox-lxc.nix"
    inputs.nixos-generators.nixosModules.all-formats
  ];

  config = lib.mkMerge [
    (lib.mkIf (config.hostConfig.feature.lxc) {
      proxmoxLXC.enable = true;

      proxmoxLXC.manageNetwork = true;
      networking.useNetworkd = true;
      networking.useHostResolvConf = false;
      networking.useDHCP = false;

      systemd.network.networks."40-lxc" = {
        matchConfig.Name = "eth0";
        dns = config.inventory.networks.home.dns;
        address = [ config.hostConfig.ipAllocation.home.primary.addressWithPrefix ];
        routes = [ { Gateway = config.inventory.networks.home.gateway; } ];
      };

      services.getty.autologinUser = "root";
    })
    # proxmox-lxc.nix enables itself by default, let's override
    (lib.mkIf (!config.hostConfig.feature.lxc) { proxmoxLXC.enable = lib.mkDefault false; })
  ];
}
