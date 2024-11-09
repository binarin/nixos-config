{ flake, ... }:
{
  inventoryHostName = "valak";
  imports = [
    flake.inputs.self.nixosModules.default
    ./configuration.nix
  ];
}
