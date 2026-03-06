{
  self,
  inputs,
  config,
  ...
}:
let
  inventoryHostName = "nix-builder-raum";
  system = "x86_64-linux";
in
{
  flake.deploy.nodes.nix-builder-raum = {
    hostname = config.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.nix-builder-raum;
    };
  };

  flake.nixosConfigurations.nix-builder-raum = inputs.nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = {
      inherit inventoryHostName;
      flake = {
        inherit self inputs config;
      };
    };
    modules = [
      self.nixosModules.nix-builder-raum-configuration
    ];
  };

  flake.nixosModules.nix-builder-raum-configuration =
    { ... }:
    {
      key = "nixos-config.modules.nixos.nix-builder-raum-configuration";
      imports = [
        "${self}/machines/nix-builder-raum/hardware-configuration.nix"

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
