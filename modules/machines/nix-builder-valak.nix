{
  self,
  inputs,
  config,
  ...
}:
let
  inventoryHostName = "nix-builder-valak";
  system = "x86_64-linux";
in
{
  flake.deploy.nodes.nix-builder-valak = {
    hostname = config.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.nix-builder-valak;
    };
  };

  flake.nixosConfigurations.nix-builder-valak = inputs.nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = {
      inherit inventoryHostName;
      flake = {
        inherit self inputs config;
      };
    };
    modules = [
      self.nixosModules.nix-builder-valak-configuration
    ];
  };

  flake.nixosModules.nix-builder-valak-configuration =
    { ... }:
    {
      key = "nixos-config.modules.nixos.nix-builder-valak-configuration";
      imports = [
        "${self}/machines/nix-builder-valak/hardware-configuration.nix"

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
