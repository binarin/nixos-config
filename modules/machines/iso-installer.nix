# modules/machines/iso-installer.nix
# Minimal NixOS installer ISO for nixos-anywhere deployments.
# This ISO is designed to be booted on Proxmox VMs and used with nixos-anywhere
# to install the target NixOS configuration.
{
  self,
  inputs,
  ...
}:
{
  flake.nixosConfigurations.iso-installer = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.iso-installer-configuration
    ];
  };

  flake.nixosModules.iso-installer-configuration =
    {
      config,
      lib,
      pkgs,
      modulesPath,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.iso-installer-configuration";

      imports = [
        # Minimal installer ISO (no graphical environment)
        "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
        "${modulesPath}/installer/cd-dvd/channel.nix"

        # Our modules for SSH keys and basic configuration
        self.nixosModules.public-keys
        self.nixosModules.nix
        self.nixosModules.ci
      ];

      config = {
        # Faster ISO builds with less compression
        isoImage.squashfsCompression = "gzip -Xcompression-level 1";

        # Simple hostname for identification
        networking.hostName = "nixos-installer";

        nixpkgs.hostPlatform = "x86_64-linux";

        # Standard state version
        system.stateVersion = "25.11";

        # Enable SSH for nixos-anywhere access
        services.openssh = {
          enable = true;
          settings.PermitRootLogin = lib.mkForce "yes";
        };
        networking.firewall.allowedTCPPorts = [ 22 ];

        # Configure root SSH access using authorized keys from inventory
        users.users.root.openssh.authorizedKeys.keys = config.lib.publicKeys.ssh.secureForUser "root";

        # Also configure nixos user for direct SSH login
        users.users.nixos.openssh.authorizedKeys.keys = config.lib.publicKeys.ssh.secureForUser "root";

        # Set simple password for console access (debugging only)
        users.users.nixos.password = "nixos";
        users.users.nixos.initialHashedPassword = lib.mkForce null;

        # Include useful tools for installation
        environment.systemPackages = with pkgs; [
          inputs.disko.packages."${config.nixpkgs.hostPlatform.system}".disko
          git
          vim
          htop
        ];

        # Enable Tailscale for remote installations
        services.tailscale.enable = true;

        # Disable sleep/suspend/hibernate for installer
        systemd.targets = {
          sleep.enable = false;
          suspend.enable = false;
          hibernate.enable = false;
          hybrid-sleep.enable = false;
        };

        # Boot optimizations for VM environment
        boot.supportedFilesystems = lib.mkForce [
          "btrfs"
          "vfat"
          "xfs"
          "zfs"
          "ext4"
        ];
      };
    };
}
