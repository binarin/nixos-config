{ self, inputs, ... }:
{
  flake.nixosModules.disko =
    { inventoryHostName, ... }:
    {
      key = "nixos-config.modules.nixos.disko";
      imports = [
        # "${self}/my-machines/${inventoryHostName}/disko.nix"
      ];
    };
}
