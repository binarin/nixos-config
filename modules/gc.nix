{
  self,
  ...
}:
{
  flake.homeModules.gc = {
    key = "nixos-config.modules.home.gc";

    # Garbage collect the Nix store
    nix.gc = {
      automatic = true;
      # Change how often the garbage collector runs (default: weekly)
      # frequency = "monthly";
    };
  };

  flake.nixosModules.gc =
    { ... }:
    {
      key = "nixos-config.modules.nixos.gc";

      config.home-manager.sharedModules = [ self.homeModules.gc ];
    };
}
