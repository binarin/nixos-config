{ self, config, ... }:
let
  flakeConfig = config;
in
{
  # Add to nixosSharedModules since it's properly gated with mkIf and `or false` fallback
  nixosSharedModules = [ self.nixosModules.lxc ];

  flake.nixosModules.lxc =
    {
      modulesPath,
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.lxc";

      # Need to add to /etc/pve/lxc/XXX.conf
      # lxc.cgroup2.devices.allow: c 10:200 rwm
      # lxc.mount.entry: /dev/net/tun dev/net/tun none bind,create=file
      imports = [
        "${modulesPath}/virtualisation/proxmox-lxc.nix"
      ];

      config = lib.mkMerge [
        (lib.mkIf (config.hostConfig.feature.lxc or false) {
          proxmoxLXC.enable = true;

          proxmoxLXC.manageNetwork = true;
          networking.useNetworkd = true;
          networking.useHostResolvConf = false;
          networking.useDHCP = false;

          systemd.network.networks."40-lxc" = {
            matchConfig.Name = "eth0";
            dns = flakeConfig.inventory.networks.home.dns;
            address = [
              flakeConfig.inventory.ipAllocation."${config.networking.hostName}".home.primary.addressWithPrefix
            ];
            routes = [ { Gateway = flakeConfig.inventory.networks.home.gateway; } ];
          };

          services.getty.autologinUser = "root";

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
        })
        # proxmox-lxc.nix enables itself by default, let's override
        (lib.mkIf (!(config.hostConfig.feature.lxc or false)) { proxmoxLXC.enable = lib.mkDefault false; })
      ];
    };
}
