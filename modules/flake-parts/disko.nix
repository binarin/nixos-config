{self, inputs, ...}: {
  flake.nixosModules.disko = {inventoryHostname, ...}: {
    key = "nixos-config.disko";
    imports = [
      inputs.disko.nixosModules.disko
      "${self}/machines/${inventoryHostname}/disko.nix"
    ];
  };
}
