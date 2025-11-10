{ ... }:
{
  flake.nixosModules.standard-linux-tools =
    {
      config,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.standard-linux-tools";

      config = {
      };
    };
}
