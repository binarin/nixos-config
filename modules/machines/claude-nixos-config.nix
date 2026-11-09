# modules/machines/claude-nixos-config.nix
{ self, inputs, ... }:
{
  flake.deploy.nodes.claude-nixos-config = {
    hostname = "claude-nixos-config";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.claude-nixos-config;
    };
  };

  flake.nixosConfigurations.claude-nixos-config = inputs.nixpkgs.lib.nixosSystem {
    pkgs = self.lib.nixpkgs;
    # system = "x86_64-linux";
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
        "${self}/my-machines/claude-nixos-config/hardware-configuration.nix"
        "${self}/my-machines/claude-nixos-config/disko.nix"
        self.nixosModules.baseline
        inputs.disko.nixosModules.disko
        self.nixosModules.systemd-boot
        self.nixosModules.impermanence
        self.nixosModules.qemu-guest
        self.nixosModules.impure-nix-setup
        self.nixosModules.binarin-workstation
        self.nixosModules.binarin-podman
        self.nixosModules.tailscale
        self.nixosModules.paseo
      ];

      config = {
        networking.hostName = "claude-nixos-config";
        networking.useNetworkd = true;
        nixos-config.qemu-guest.proxmox.network.inventoryNetwork = "guest";
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

        services.paseo = {
          enable = true;
          user = "binarin";
          group = "binarin";
          hostnames = [ "claude-nixos-config.lynx-lizard.ts.net" ];
          environment.PASEO_WEB_UI_ENABLED = "true";
          relay.enable = false;
        };

        services.tailscale.serve = {
          enable = true;
          configs.paseo.target = "localhost:6767";
        };

      };
    };
}
