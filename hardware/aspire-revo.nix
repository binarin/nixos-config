{ config, lib, pkgs, ... }:

{
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci" ];
  # boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableAllFirmware = true;
  nix.maxJobs = 2;
  boot.kernelParams = [ "nofb" "nomodeset" "vga=normal" ];
  services.xserver.videoDrivers = [ "nvidiaLegacy340" ];
}
