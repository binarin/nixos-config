{
  self,
  inputs,
  ...
}:
{
  flake.nixosConfigurations.ishamael = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.ishamael-configuration
    ];

  };

  flake.nixosModules.ishamael-configuration =
    {
      config,
      pkgs,
      modulesPath,
      ...
    }:
    {
      key = "nixos-config.ishamael-configuration";
      imports = [
        (modulesPath + "/profiles/qemu-guest.nix")
        self.nixosModules.default
        self.nixosModules.user-binarin
        self.nixosModules.hyprland
        self.nixosModules.impure-nix-setup
      ];

      config = {
        networking.hostName = "ishamael";

        # Move XDG directories to .xdg subdirectory
        environment.sessionVariables = {
          XDG_CACHE_HOME = "$HOME/.xdg/cache";
          XDG_CONFIG_HOME = "$HOME/.xdg/config";
          XDG_DATA_HOME = "$HOME/.xdg/local/share";
          XDG_STATE_HOME = "$HOME/.xdg/local/state";
        };

        home-manager.users.binarin =
          { config, ... }:
          {
            xdg = {
              enable = true;
              cacheHome = "${config.home.homeDirectory}/.xdg/cache";
              configHome = "${config.home.homeDirectory}/.xdg/config";
              dataHome = "${config.home.homeDirectory}/.xdg/local/share";
              stateHome = "${config.home.homeDirectory}/.xdg/local/state";
            };
          };

        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;

        boot.initrd.availableKernelModules = [
          "uhci_hcd"
          "ehci_pci"
          "ahci"
          "virtio_pci"
          "virtio_scsi"
          "sd_mod"
          "sr_mod"
        ];
        boot.initrd.kernelModules = [ ];
        boot.kernelModules = [ "kvm-amd" ];
        boot.extraModulePackages = [ ];
        boot.kernelPackages = pkgs.linuxPackages_latest;

        hardware.enableAllFirmware = true;
        hardware.graphics = {
          enable = true;
          extraPackages = with pkgs; [
            intel-media-driver
            intel-ocl
            intel-vaapi-driver
            vpl-gpu-rt
          ];
        };

        fileSystems."/" = {
          device = "/dev/disk/by-uuid/c2e9bb0f-3517-45dd-87f5-cc8cd2cd7353";
          fsType = "ext4";
        };

        fileSystems."/boot" = {
          device = "/dev/disk/by-uuid/48DE-5AE4";
          fsType = "vfat";
          options = [
            "fmask=0077"
            "dmask=0077"
          ];
        };
        networking.useDHCP = false;

        systemd.network = {
          enable = true;
          networks = {
            "40-enp6s18" =
              let
                inherit (config.inventory.hostIpAllocation.home.primary) addressWithPrefix;
                inherit (config.inventory.networks.home) gateway dns;
              in
              {
                matchConfig.Name = "enp6s18";

                address = [ addressWithPrefix ];
                routes = [ { Gateway = gateway; } ];

                dns = dns;
                bridgeConfig = { };
                linkConfig = {
                  RequiredForOnline = "routable";
                };
              };
          };
        };

        swapDevices = [ ];

        # Enable the KDE Plasma Desktop Environment.
        services.displayManager.sddm.wayland.enable = true;
        services.displayManager.sddm.enable = true;
        services.displayManager.autoLogin.enable = true;
        services.displayManager.autoLogin.user = "binarin";
        services.desktopManager.plasma6.enable = true;

        services.openssh.enable = true;

        users.users."binarin".openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMCVAKqmUdCkJ1gbi2ZA6vLnmf880U/9v5bfxhChapWB binarin@nixos"
        ];

        services.sunshine = {
          enable = true;
          capSysAdmin = true;
        };

        system.stateVersion = "24.11";
      };
    };
}
