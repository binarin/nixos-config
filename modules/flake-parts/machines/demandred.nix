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
      self.nixosModules.demandred-configuration
    ];
  };

  flake.nixosModules.demandred-configuration = {config, lib, pkgs, ...}: {
    key = "nixos-config.demandred-configuration";
    imports = [
      self.nixosModules.kanata
      self.nixosModules.niri
    ];
    config = {
      services.kanata.keyboards.all.devices = [
        "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
      ];
    };
  };
}
