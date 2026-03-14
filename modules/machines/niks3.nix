{ self, config, ... }:
let
  flakeConfig = config;
  inventoryHostName = "niks3";
in
{
  clan.inventory.machines.niks3 = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
  };

  clan.machines.niks3 = {
    imports = [
      self.nixosModules.niks3-configuration
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
  };

  flake.nixosModules.niks3-configuration =
    { ... }:
    {
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
      ];

      nixos-config.export-metrics.enable = false;
    };
}
