{ self, inputs, ... }:
{
  flake.nixosModules.disko =
    { inventoryHostName, ... }:
    {
      key = "nixos-config.disko";
      imports = [
        inputs.disko.nixosModules.disko
        "${self}/machines/${inventoryHostName}/disko.nix"
      ];
    };
}
