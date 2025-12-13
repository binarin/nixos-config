{
  self,
  inputs,
  ...
}:
{
  flake.nixosConfigurations.demandred = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.demandred-configuration
    ];
  };

  flake.nixosModules.demandred-configuration =
    {
      lib,
      pkgs,
      modulesPath,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.demandred-configuration";
      imports = [
        self.nixosModules.baseline

        (modulesPath + "/installer/scan/not-detected.nix")
        inputs.disko.nixosModules.default
        "${self}/machines/demandred/disko.nix"

        self.nixosModules.default
        self.nixosModules.binarin-workstation
        self.nixosModules.nix
        self.nixosModules.kanata
        self.nixosModules.niri
        self.nixosModules.bluetooth
        self.nixosModules.firefox
        self.nixosModules.inventory-legacy
        self.nixosModules.impure-nix-setup
        self.nixosModules.large-console-fonts
      ];
      config = {
        nixos-config.export-metrics.enable = false;
        networking.hostName = "demandred";

        impermanence.enable = true;

        system.stateVersion = "24.11";

        boot.initrd.availableKernelModules = [
          "xhci_pci"
          "ehci_pci"
          "ahci"
          "usb_storage"
          "sd_mod"
          "sr_mod"
          "sdhci_pci"
        ];
        boot.initrd.kernelModules = [ "dm-snapshot" ];
        boot.kernelModules = [ "kvm-intel" ];
        boot.extraModulePackages = [ ];

        boot.initrd.systemd.enable = true;
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;

        services.tailscale.authKeyFile = lib.mkForce null;
        boot.initrd.luks.devices.demandred-lvm.crypttabExtraOpts = [
          "fido2-device=auto"
          "token-timeout=10s"
        ];

        fileSystems."/persist".neededForBoot = true;
        fileSystems."/local".neededForBoot = true;

        networking.networkmanager.enable = true;
        networking.useDHCP = lib.mkDefault true;

        hardware.cpu.intel.updateMicrocode = lib.mkDefault true;

        users.users.root.initialHashedPassword = "$7$CU..../....2tYl/rrPqgcDE/0wbfkSR/$BDDtkNKdAi/yfv3P7ETmpoCKBxfHdiRIM8B4K8nFuB3";
        users.users.binarin.initialHashedPassword = "$7$CU..../....w.WruOOmFL2KKwVMMMysm1$OxbByS3HBVRsOdmYlqBtpivURr1QWVBVf87M1gXAEQC";

        services.kanata.keyboards.all.devices = [
          "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
        ];

        environment.systemPackages = with pkgs; [
          distrobox
        ];

        virtualisation.docker = {
          enable = true;
          storageDriver = "btrfs";
        };

        services.displayManager.defaultSession = lib.mkForce "niri-uwsm";
      };
    };
}
