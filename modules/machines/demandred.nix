{
  self,
  inputs,
  config,
  ...
}:
{
  flake.nixosConfigurations.demandred = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      flake = {
        inherit self inputs config;
      };
      hostConfig = {
        isLinux = true;
      };
    };
    modules = [
      self.nixosModules.demandred-configuration
    ]
    ++ self.nixosSharedModules;
  };

  flake.nixosModules.demandred-configuration =
    {
      config,
      lib,
      pkgs,
      modulesPath,
      ...
    }:
    {
      key = "nixos-config.demandred-configuration";
      imports = [
        (modulesPath + "/installer/scan/not-detected.nix")
        inputs.disko.nixosModules.default
        "${self}/configurations/nixos/demandred/disko-config.nix"

        self.nixosModules.default
        self.nixosModules.user-binarin
        self.nixosModules.nix
        self.nixosModules.kanata
        self.nixosModules.niri
        self.nixosModules.bluetooth
        self.nixosModules.firefox
        self.nixosModules.hyprland
        self.nixosModules.inventory-legacy
      ];
      config = {
        networking.hostName = "demandred";

        impermanence.enable = true;

        hostConfig.features = [
          "hyprland"
          "interactive-cli"
          "emacs"
          "workstation"
          "tailscale"
          # "airgapped"
          "nix-builder"
        ];

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

        console.useLargeFonts = true;

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
