{
  self,
  config,
  lib,
  inputs,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
in
{
  clan.inventory.machines.llm-runner = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.llm-runner.home.primary.address;
  };

  clan.machines.llm-runner = {
    imports = [
      self.nixosModules.llm-runner-configuration
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
  };

  flake.nixosConfigurations.llm-runner = lib.mkForce (
    self.clan.nixosConfigurations.llm-runner.extendModules {
      specialArgs.inventoryHostName = "llm-runner";
    }
  );

  flake.nixosModules.llm-runner-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.llm-runner-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.qemu-guest
        self.nixosModules.disko-template-zfs-whole
        (selfLib.file' "machines/llm-runner/hardware-configuration.nix")
      ];

      nixos-config.export-metrics.enable = false;

      nixos-config.qemu-guest.proxmox = {
        memory = 65536;
        balloon = 2048;
        cores = 16;
        bios = "ovmf";
        machine = "q35";
        description = "LLM runner";
        pci-passthrough = {
          nvme = {
            id = "Samsung Electronics Co Ltd NVMe SSD Controller SM981/PM981/PM983";
            bootable = true;
          };
          gpu = {
            id = "NVIDIA Corporation GA102 [GeForce RTX 3090] (rev a1)";
            rom = selfLib.file "GA102.rom.git-crypt";
          };
          gpu-sound = {
            id = "NVIDIA Corporation GA102 High Definition Audio Controller (rev a1)";
          };
          # radeon = {
          #   mapping = "large-radeon";
          # };
        };
      };

      impermanence.enable = true;
      disko.devices.disk.main.device =
        "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_1TB_S4EWNF0M723324Z";

      services.xserver.videoDrivers = [ "nvidia" ];
      services.xserver.deviceSection = ''
        Section "Device"
          Identifier "nvidia-undervolt"
          Driver "nvidia"
          Option "Coolbits" "28"
        EndSection
      '';

      hardware.nvidia = {
        modesetting.enable = true;
        powerManagement.enable = true;
        open = false;
        nvidiaSettings = true;
        package = config.boot.kernelPackages.nvidiaPackages.latest;
      };

      hardware.nvidia-container-toolkit.enable = true;
      hardware.graphics.enable = true;
      hardware.graphics.enable32Bit = true;

      nixpkgs.config = {
        cudaSupport = true;
        packageOverrides = pkgs: {
          llama-cpp = pkgs.callPackage "${inputs.nixpkgs-unstable}/pkgs/by-name/ll/llama-cpp/package.nix" { };
          llama-swap =
            pkgs.callPackage "${inputs.nixpkgs-unstable}/pkgs/by-name/ll/llama-swap/package.nix"
              { };
        };
      };

      services.llama-swap = {
        enable = true;
        settings = {
          models = {
            "qwen3-coder:30b" = {
              cmd = ''
                ${lib.getExe' pkgs.llama-cpp "llama-server"} --hf-repo unsloth/Qwen3-Coder-30B-A3B-Instruct-GGUF:Q4_K_M --port ''${PORT} --ctx-size 131072
              '';
            };
          };
        };
      };
    };
}
