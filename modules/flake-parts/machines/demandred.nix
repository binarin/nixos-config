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
      self.nixosModules.demandred-configuration
      ../../../configurations/nixos/demandred/configuration.nix
    ];
  };

  flake.nixosModules.demandred-configuration = {config, lib, pkgs, ...}: {
    key = "nixos-config.demandred-configuration";
    imports = [
      self.nixosModules.nix
      self.nixosModules.kanata
      self.nixosModules.niri
      self.nixosModules.bluetooth
      self.nixosModules.inventory-legacy
    ];
    config = {
      services.kanata.keyboards.all.devices = [
        "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
      ];

      virtualisation.docker = {
        enable = true;
        storageDriver = "btrfs";
      };

      services.displayManager.defaultSession = lib.mkForce "niri-uwsm";
    };
  };
}
