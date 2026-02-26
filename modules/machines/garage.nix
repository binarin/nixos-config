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
        inherit self inputs config;
      };
    };
    modules = [
      self.nixosModules.garage-configuration
    ];
  };

  flake.nixosModules.garage-configuration =
    { ... }:
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

      };
    };
}
