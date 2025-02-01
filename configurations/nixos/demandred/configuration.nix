# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  nix.usePersonalNixCache = false;

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.initrd.systemd.enable = true;

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/EE30-EA3C";
      fsType = "vfat";
      options = [ "fmask=0077" "dmask=0077" ];
    };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = {
    luks-rpool-1.device = "/dev/disk/by-partlabel/demandred-luks-rpool-1";
    luks-swap-1.device = "/dev/disk/by-partlabel/demandred-luks-swap-1";
  };

  swapDevices = [ { device = "/dev/mapper/luks-swap-1"; } ];

  networking.networkmanager.enable = true;
  networking.useDHCP = lib.mkDefault true;

  hardware.cpu.intel.updateMicrocode = lib.mkDefault true;

  services.tailscale.enable = true;
  services.desktopManager.plasma6.enable = true;
  services.displayManager.sddm.enable = true;

  users.users.root.initialHashedPassword = "$7$CU..../....2tYl/rrPqgcDE/0wbfkSR/$BDDtkNKdAi/yfv3P7ETmpoCKBxfHdiRIM8B4K8nFuB3";
  users.users.binarin.initialHashedPassword = "$7$CU..../....2tYl/rrPqgcDE/0wbfkSR/$BDDtkNKdAi/yfv3P7ETmpoCKBxfHdiRIM8B4K8nFuB3";
}
