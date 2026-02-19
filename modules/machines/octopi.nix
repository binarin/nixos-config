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
        self.nixosModules.baseline
        # self.nixosModules.flake-files
        # self.nixosModules.inventory
        # self.nixosModules.inventory-legacy
        # self.nixosModules.nix
        # self.nixosModules.sshd
        # self.nixosModules.security
        # self.nixosModules.sops
        # self.nixosModules.tailscale
      ];

      networking.hostName = "octopi";
      system.stateVersion = "25.11";

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

      users.users.root.openssh.authorizedPrincipals = [
        "root"
        "binarin"
      ];

      networking.networkmanager.enable = true;
      networking.networkmanager.ensureProfiles = {
        profiles = {
          agares-guest = {
            connection = {
              id = "agares-guest";
              type = "wifi";
            };
            ipv4 = {
              method = "auto";
            };
            ipv6 = {
              addr-gen-mode = "stable-privacy";
              method = "auto";
            };
            wifi = {
              mode = "infrastructure";
              ssid = "agares-guest";
            };
            wifi-security = {
              key-mgmt = "wpa-psk";
              psk = config.lib.self.read "agares-guest.git-crypt";
            };
          };
        };
      };

      networking.firewall.enable = true;
      nixos-config.export-metrics.enable = false;

      nixpkgs.buildPlatform = "x86_64-linux";
      nixpkgs.hostPlatform = "aarch64-linux";
    };
}
