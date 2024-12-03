# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

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
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;
}
