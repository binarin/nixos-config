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
    pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
    # inherit system;
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
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.nix-builder-raum-configuration";
      imports = [
        "${self}/my-machines/nix-builder-raum/hardware-configuration.nix"

        self.nixosModules.lxc
        self.nixosModules.baseline
        self.nixosModules.nix-builder
      ];

      config = {
        networking.hostName = inventoryHostName;
        system.stateVersion = "25.11";

        nixos-config.nix-builder.runnerCount = 2;

        proxmoxLXC = {
          cores = 8;
          memory = 32768;
          mounts = [
            {
              mountPoint = "/nix";
              size = "512G";
            }
            {
              mountPoint = "/var/lib/private/gitea-runner";
              size = "32G";
            }
          ];
        };
      };
    };
}
