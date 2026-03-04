{
  self,
  inputs,
  config,
  ...
}:
let
  inventoryHostName = "garage";
  system = "x86_64-linux";
in
{
  flake.deploy.nodes.garage = {
    hostname = config.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.garage;
    };
  };

  flake.nixosConfigurations.garage = inputs.nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = {
      inherit inventoryHostName;
      flake = {
        inherit self inputs;
      };
    };
    modules = [
      self.nixosModules.garage-configuration
    ];
  };

  flake.nixosModules.garage-configuration =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.nixos.garage-configuration";
      imports = [
        "${self}/machines/garage/hardware-configuration.nix"

        self.nixosModules.lxc

        self.nixosModules.baseline

      ];

      config = {
        networking.hostName = inventoryHostName;
        system.stateVersion = "25.11";
        nixos-config.export-metrics.enable = false;

        proxmoxLXC.mounts = [
          {
            # /nix mount - uses default pool (local-zfs)
            mountPoint = "/nix";
            size = "32G";
          }
          {
            # garage data mount - on spinning-zfs, 1TB
            pool = "spinning-zfs";
            mountPoint = "/var/lib/garage/data";
            size = "1T";
          }
        ];

        services.garage = {
          enable = true;
          package = pkgs.garage;
          settings = {
            metadata_dir = "/var/lib/garage/meta";
            data_dir = "/var/lib/garage/data";
          };
        };
      };
    };
}
