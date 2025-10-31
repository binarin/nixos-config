{ self, inputs, config, ... }:
{
  flake.nixosConfigurations.demandred = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      flake = {
        inherit self inputs config;
      };
      hostConfig = {
        isLinux = true;
        isDarwin = false;
      };
    };
    modules = [
      ../../../configurations/nixos/demandred/configuration.nix
      self.nixosModules.kanata
    ];
  };
}
