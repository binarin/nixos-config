{
  self,
  config,
  lib,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
  xrayLib = import "${self}/lib/xray/config-lib.nix";
in
{
  flake.deploy.nodes.xray-exit = {
    hostname = flakeConfig.inventory.ipAllocation.xray-exit.guest.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.xray-exit;
    };
  };

  clan.inventory.machines.xray-exit = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.xray-exit.guest.primary.address;
  };

  clan.machines.xray-exit = {
    imports = [ self.nixosModules.xray-exit-configuration ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosConfigurations.xray-exit = lib.mkForce (
    self.clan.nixosConfigurations.xray-exit.extendModules {
      specialArgs.inventoryHostName = "xray-exit";
    }
  );

  flake.nixosModules.xray-exit-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.xray-exit-configuration";

      imports = [
        self.nixosModules.nixos-base
        self.nixosModules.qemu-guest
        (selfLib.file' "machines/llm-runner/hardware-configuration.nix")

        # self.nixosModules.sops
        self.nixosModules.xray-shared
        self.nixosModules.provision-clan-key
      ];

      fileSystems."/" = {
        device = "/dev/disk/by-label/nixos";
        fsType = "ext4";
        autoResize = true;
      };
      fileSystems."/boot" = {
        device = "/dev/disk/by-label/ESP";
        fsType = "vfat";
        # Without these the ESP mounts 0022, and bootctl warns that the mount
        # point backing /boot/loader/random-seed is world accessible.
        options = [
          "fmask=0077"
          "dmask=0077"
        ];
      };

      boot.growPartition = true;

      networking.useNetworkd = true;

      boot.initrd.provisionClanKey.enable = true;

      system.build.cloudImage = import "${pkgs.path}/nixos/lib/make-disk-image.nix" {
        inherit lib pkgs config;
        format = "qcow2";
        partitionTableType = "efi";
        additionalSpace = "1024M";
      };

      nixos-config.qemu-guest.proxmox = {
        memory = 1024;
        network.inventoryNetwork = "guest";
        balloon = 512;
        cores = 2;
        bios = "ovmf";
        machine = "q35";
        description = "xray-exit (residential VPN exit)";
        disks = [
          {
            type = "image";
            storage = "local-zfs";
            size = "12G";
            bootOrder = 1;
          }
        ];
      };

      # Public REALITY inbound reachable via the router port-forward.
      networking.firewall.allowedTCPPorts = [ 8443 ];

      # sops.templates."xray.json" = {
      #   restartUnits = [ "xray.service" ];
      #   content = builtins.toJSON (
      #     xrayLib.mkExitSettings {
      #       linkId = "xray-link/uuid";
      #       exitDest = "xray-exit-params/dest";
      #       exitSni = "xray-exit-params/sni";
      #       exitPrivateKey = "xray-reality-exit/private-key";
      #       exitShortId = val "xray-reality-exit" "short-id";
      #     }
      #   );
      # };

      # services.xray = {
      #   enable = true;
      #   settingsFile = config.sops.templates."xray.json".path;
      # };
    };
}
