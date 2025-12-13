{
  self,
  inputs,
  ...
}:
{
  flake.nixosConfigurations.devcontainer = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.devcontainer-configuration
    ];
  };

  flake.nixosModules.devcontainer-configuration =
    {
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.devcontainer-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.impermanence
        {
          home-manager.users.binarin = self.homeModules.claude-code;
        }
      ];

      config = {
        nixos-config.export-metrics.enable = false;
        boot.isContainer = true;

        console.enable = true;
        security.sudo.enable = lib.mkForce false;

        networking.useNetworkd = true;
        networking.useHostResolvConf = false;

        services.getty.autologinUser = "binarin";

        nix.extraOptions = ''
          experimental-features = nix-command flakes
        '';

        networking.hostName = "devcontainer";

        system.stateVersion = "25.05";
      };
    };
}
