{ flake, modulesPath, config, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  inherit (config.hostOptions.ipamConfig) interfaces;
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
    dns = interfaces.eth0.network.dns;
    address = [ "${interfaces.eth0.address}/${toString interfaces.eth0.network.prefix}" ];
    routes = [
      { routeConfig.Gateway = interfaces.eth0.network.gateway; }
    ];
  };

  services.getty.autologinUser = "root";
}
