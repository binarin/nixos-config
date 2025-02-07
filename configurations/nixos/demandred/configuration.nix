# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{ flake, config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [
      (modulesPath + "/installer/scan/not-detected.nix")
      flake.inputs.disko.nixosModules.default
      ./disko-config.nix
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.initrd.systemd.enable = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.tailscale.authKeyFile = lib.mkForce null;
  boot.initrd.luks.devices.demandred-lvm.crypttabExtraOpts = [ "fido2-device=auto" "token-timeout=10s" ];

  fileSystems."/persist".neededForBoot = true;
  fileSystems."/local".neededForBoot = true;

  networking.networkmanager.enable = true;
  networking.useDHCP = lib.mkDefault true;

  hardware.cpu.intel.updateMicrocode = lib.mkDefault true;

  users.users.root.initialHashedPassword = "$7$CU..../....2tYl/rrPqgcDE/0wbfkSR/$BDDtkNKdAi/yfv3P7ETmpoCKBxfHdiRIM8B4K8nFuB3";
  users.users.binarin.initialHashedPassword = "$7$CU..../....w.WruOOmFL2KKwVMMMysm1$OxbByS3HBVRsOdmYlqBtpivURr1QWVBVf87M1gXAEQC";

  console.useLargeFonts = true;
}
