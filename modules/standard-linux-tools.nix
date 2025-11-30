{ ... }:
{
  flake.nixosModules.standard-linux-tools =
    {
      ...
    }:
    {
      key = "nixos-config.modules.nixos.standard-linux-tools";

      config = {
      };
    };
}
