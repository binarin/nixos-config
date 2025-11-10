{
  self,
  inputs,
  ...
}:
{
  flake.nixosConfigurations.iso = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.iso-configuration
    ];
  };

  flake.nixosModules.iso-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.iso-configuration";
      imports = [
        "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
        "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
        self.nixosModules.default
        self.nixosModules.impure-nix-setup
        self.nixosModules.user-binarin
        self.nixosModules.large-console-fonts
      ];

      config = {
        networking.hostName = "iso";

        nixpkgs.hostPlatform = "x86_64-linux";

        # NOTE: Don't bump without re-installing complete system!
        system.stateVersion = "24.11";

        boot = {
          supportedFilesystems = lib.mkForce [
            "btrfs"
            "reiserfs"
            "vfat"
            "f2fs"
            "xfs"
            "ntfs"
            "cifs"
            "zfs"
            "exfat"
          ];
        };

        services.openssh.enable = true;
        services.openssh.settings.PermitRootLogin = lib.mkForce "yes";

        services.tailscale.enable = true;

        environment.systemPackages = with pkgs; [
          inputs.disko.packages."${config.nixpkgs.hostPlatform.system}".disko
        ];

        users.users.nixos.password = "nixos";
        users.users.nixos.initialHashedPassword = lib.mkForce null;

        systemd = {
          targets = {
            sleep.enable = false;
            suspend.enable = false;
            hibernate.enable = false;
            hybrid-sleep.enable = false;
          };
        };
      };
    };
}
