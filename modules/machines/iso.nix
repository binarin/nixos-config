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
      key = "nixos-config.modules.nixos.iso-configuration";
      imports = [
        "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
        "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"

        self.nixosModules.baseline
        self.nixosModules.microsoft-surface
        self.nixosModules.sshd
        self.nixosModules.kanata
        self.nixosModules.large-console-fonts
      ];

      config = {
        nixos-config.export-metrics.enable = false;

        # build faster
        isoImage.squashfsCompression = "gzip -Xcompression-level 1";

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
        services.openssh.authorizedKeysInHomedir = lib.mkForce true;

        services.tailscale.enable = true;

        environment.systemPackages = with pkgs; [
          inputs.disko.packages."${config.nixpkgs.hostPlatform.system}".disko
        ];

        users.users.nixos.password = "nixos";
        users.users.nixos.initialHashedPassword = lib.mkForce null;
        users.users.nixos.openssh.authorizedPrincipals = [
          "root"
          "binarin"
        ];

        users.users.root.openssh.authorizedPrincipals = [
          "root"
          "binarin"
        ];

        networking.networkmanager.enable = true;
        # WiFi credentials should be injected post-build to avoid git-crypt dependency during evaluation
        # Use scripts/inject-iso-wifi.sh to add WiFi profile to built ISO

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
