{ flake, modulesPath, config, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    "${modulesPath}/virtualisation/proxmox-lxc.nix"
  ];

  proxmoxLXC.enable = true;

  proxmoxLXC.manageNetwork = true;
  networking.useNetworkd = true;
  networking.useHostResolvConf = false;
  networking.useDHCP = false;

  systemd.network.networks."40-eth0" = {
    matchConfig.Name = "eth0";
    dns = config.inventory.networks.home.dns;
    address = [ config.hostConfig.ipAllocation.home.primary.addressWithPrefix ];
    routes = [
      { routeConfig.Gateway = config.inventory.networks.home.gateway; }
    ];
  };

  services.getty.autologinUser = "root";
}
