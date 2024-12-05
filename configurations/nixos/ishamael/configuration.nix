# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/profiles/qemu-guest.nix")
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ahci" "virtio_pci" "virtio_scsi" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  boot.kernelPackages = pkgs.linuxPackages_latest;

  hardware.enableAllFirmware = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/c2e9bb0f-3517-45dd-87f5-cc8cd2cd7353";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/48DE-5AE4";
      fsType = "vfat";
      options = [ "fmask=0077" "dmask=0077" ];
    };
    networking.useDHCP = false;

    systemd.network = {
      enable = true;
      networks = {
        "40-enp6s18" =
          let
            inherit (config.hostConfig.ipAllocation.home.primary) addressWithPrefix;
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

}
