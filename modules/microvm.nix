{ self, inputs, ... }:
{
  flake-file.inputs.microvm = {
    url = "github:microvm-nix/microvm.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  flake.nixosModules.use-microvms =
    { ... }:
    {
      key = "nixos-config.modules.nixos.use-microvms";
      imports = [
        inputs.microvm.nixosModules.host
      ];
      config = {
        networking.useNetworkd = true;
        systemd.network = {
          netdevs."10-microvm".netdevConfig = {
            Kind = "bridge";
            Name = "microvm";
          };
          networks."10-microvm" = {
            matchConfig.Name = "microvm";
            networkConfig = {
              ConfigureWithoutCarrier = true;
              DHCP = false;
              DHCPServer = false;
              IPv6AcceptRA = false;
            };
            linkConfig.RequiredForOnline = false;
            addresses = [
              {
                Address = "192.168.83.1/24";
                DuplicateAddressDetection = "none";
              }
            ];
          };
          networks."11-microvm" = {
            matchConfig.Name = "microvm-*";
            networkConfig.Bridge = "microvm";
          };
        };
      };
    };

  flake.nixosConfigurations.microvm-nixos-config = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inventoryHostName = "furfur";
    };
    modules = [
      self.nixosModules.microvm-nixos-config-configuration
    ];
  };

  flake.nixosModuels.microvm-nixos-config-configuration =
    { ... }:
    {
      modules = [
        inputs.microvm.nixosModules.microvm
        self.nixosModules.baseline
        {
          microvm.hypervisor = "cloud-hypervisor";
        }
      ];
    };

}
