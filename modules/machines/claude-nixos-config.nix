# modules/machines/claude-nixos-config.nix
{ self, inputs, ... }:
{
  flake.deploy.nodes.claude-nixos-config = {
    # hostname = "claude-nixos-config";
    # hostname = "claude-nixos-config";
    hostname = "192.168.3.192";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.claude-nixos-config;
    };
  };

  flake.nixosConfigurations.claude-nixos-config = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inventoryHostName = "claude-nixos-config";
    };
    modules = [
      self.nixosModules.claude-nixos-config-configuration
    ];
  };

  flake.nixosModules.claude-nixos-config-configuration =
    { ... }:
    {
      key = "nixos-config.modules.nixos.claude-nixos-config-configuration";
      imports = [
        "${self}/machines/claude-nixos-config/hardware-configuration.nix"
        self.nixosModules.baseline
        self.nixosModules.disko
        self.nixosModules.systemd-boot
        self.nixosModules.impermanence
        self.nixosModules.qemu-guest
        self.nixosModules.impure-nix-setup
        self.nixosModules.binarin-workstation
        self.nixosModules.tailscale
      ];

      config = {
        networking.hostName = "claude-nixos-config";
        system.stateVersion = "25.11";
        nixos-config.export-metrics.enable = false;
        impermanence.enable = true;
        fileSystems."/persist".neededForBoot = true;
        fileSystems."/local".neededForBoot = true;
        services.getty.autologinUser = "binarin";
        home-manager.users.binarin =
          { ... }:
          {
            programs.git.settings.credential.helper = "store";
          };
      };
    };
}
