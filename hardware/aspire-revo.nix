{ config, lib, pkgs, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_4_14.extend(pSelf: pSuper: {
    legacy_340 = pSuper.legacy_340.overrideAttrs(oldAttrs: {
      patches = [ ../nixpkgs/pkgs/os-specific/linux/nvidia-x11/vm_operations_struct-fault.patch ];
    });
  });
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci" ];
  # boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableAllFirmware = true;
  nix.maxJobs = 2;
  boot.kernelParams = [ "nofb" "nomodeset" "vga=normal" ];
  services.xserver.videoDrivers = [ "nvidiaLegacy340" ];
}
