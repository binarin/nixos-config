{
  self,
  inputs,
  config,
  ...
}:
{
  flake.deploy.nodes.octopi = {
    hostname = config.inventory.ipAllocation."octopi".guest.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.octopi;
    };
  };

  flake.nixosConfigurations.octopi = inputs.nixos-raspberrypi.lib.nixosSystem {
    specialArgs = {
      nixos-raspberrypi = inputs.nixos-raspberrypi;
      inventoryHostName = "octopi";
    };
    modules = [
      (
        { modulesPath, lib, ... }:
        {
          imports =
            (with inputs.nixos-raspberrypi.nixosModules; [
              raspberry-pi-4.base
              raspberry-pi-4.display-vc4
              raspberry-pi-4.bluetooth
            ])
            ++ [
              # Re-add essential alias from rename.nix
              (lib.mkAliasOptionModule [ "environment" "checkConfigurationOptions" ] [ "_module" "check" ])
            ];
          # Remove the conflicting mkRemovedOptionModule from rename.nix
          disabledModules = [
            (modulesPath + "/rename.nix")
          ];
        }
      )
      self.nixosModules.octopi-configuration
    ];
  };

  flake.nixosModules.octopi-configuration =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.octopi-configuration";

      imports = [
        self.nixosModules.inventory
        self.nixosModules.inventory-legacy
        self.nixosModules.nix
        self.nixosModules.sshd
        self.nixosModules.security
        self.nixosModules.sops
        self.nixosModules.tailscale
      ];

      networking.hostName = "octopi";
      system.stateVersion = "25.05";

      # SD card filesystem layout
      fileSystems."/" = {
        device = "/dev/disk/by-label/NIXOS_SD";
        fsType = "ext4";
      };

      fileSystems."/boot/firmware" = {
        device = "/dev/disk/by-label/FIRMWARE";
        fsType = "vfat";
        options = [
          "nofail"
          "noauto"
        ];
      };

      users.users.root.openssh.authorizedPrincipals = [ "octopi" ];

      networking.useDHCP = false;

      systemd.network = {
        enable = true;
        networks."10-eth0" = {
          matchConfig.Name = "end0";
          dns = config.inventory.networks.guest.dns;
          address = [
            config.inventory.ipAllocation."${config.networking.hostName}".guest.primary.addressWithPrefix
          ];
          routes = [ { Gateway = config.inventory.networks.guest.gateway; } ];
          linkConfig.RequiredForOnline = "routable";
        };
      };

      networking.firewall.enable = true;
    };
}
