{ self, inputs, ... }:
{
  flake.nixosModules.disko =
    { inventoryHostName, ... }:
    {
      key = "nixos-config.modules.nixos.disko";
      imports = [
        inputs.disko.nixosModules.disko
        "${self}/my-machines/${inventoryHostName}/disko.nix"
      ];
    };
}
