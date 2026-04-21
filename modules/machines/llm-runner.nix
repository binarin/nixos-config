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
  flake.deploy.nodes.llm-runner = {
    hostname = "llm-runner";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.llm-runner;
    };
  };

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

  flake.nixosModules.llama-models =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.llama-models;
    in
    {
      key = "nixos-config.modules.nixos.llama-models";
      imports = [
      ];
      options.llama-models = with lib; {
        dir = mkOption {
          type = types.str;
          default = "/var/lib/llama-models";
        };
        models = mkOption {
          type = types.attrsOf (
            types.submodule (
              { name, config, ... }:
              {
                options = {
                  name = mkOption {
                    type = types.str;
                    default = name;
                  };
                  url = mkOption {
                    type = types.str;
                  };
                  filename = mkOption {
                    type = types.str;
                    default = baseNameOf config.url;
                  };
                  path = mkOption {
                    type = types.str;
                    default = "${cfg.dir}/${config.filename}";
                  };
                };
              }
            )
          );
        };
        configurations = mkOption {
          type = types.attrsOf (
            types.submodule (
              { name, config, ... }:
              {
                options = {
                  name = mkOption {
                    type = types.str;
                    default = name;
                  };
                  model = mkOption {
                    type = types.str;
                    default = name;
                  };
                  settings = mkOption {
                    type = types.submodule (
                      let
                        intOpt =
                          def:
                          lib.mkOption {
                            type = with lib.types; int;
                            default = def;
                          };
                        floatOpt =
                          def:
                          lib.mkOption {
                            type =
                              with lib.types;
                              oneOf [
                                str
                                int
                                float
                              ];
                            default = def;
                          };
                      in
                      {
                        options = {
                          ctx-size = intOpt 4096;
                          temp = floatOpt 0.8;
                          top-p = floatOpt 0.9;
                          top-k = intOpt 40;
                          repeat-penalty = floatOpt 1.00;
                        };
                      }
                    );
                  };
                };
              }
            )
          );
        };
      };
      config = {
        llama-models.dir = lib.mkIf config.impermanence.enable "/persist/var/lib/llama-models";

        systemd.services."llama-models-download" =
          let
            modelSnippet =
              {
                url,
                filename,
                path,
                ...
              }:
              let
                cleanPath = lib.escapeShellArg path;
              in
              ''
                dest=${cleanPath}
                if [[ ! -f "$dest" ]]; then
                    tmp_file="$dest.tmp"
                    if aria2c --max-connection-per-server=4 --min-split-size=1M --out "$tmp_file" ${lib.escapeShellArg url}; then
                      mv "$tmp_file" "$dest"
                    fi
                fi
              '';
          in
          {
            path = with pkgs; [
              coreutils
              aria2
            ];
            wantedBy = [ "multi-user.target" ];
            script =
              with lib;
              ''
                install -d "${cfg.dir}" -m 0755
              ''
              + pipe cfg.models [
                attrValues
                (map modelSnippet)
                (concatStringsSep "\n")
              ];
            serviceConfig = {
              Type = "exec";
              RemainAfterExit = true;
            };
          };
      };
    };

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
        self.nixosModules.disko-template-separate-uefi-boot
        self.nixosModules.llama-models
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
        disks = [
          {
            type = "image";
            storage = "local-zfs";
            size = "128M";
            bootOrder = 1;
          }
        ];

        pci-passthrough = {
          nvme = {
            id = "Samsung Electronics Co Ltd NVMe SSD Controller SM981/PM981/PM983";
          };
          gpu = {
            id = "NVIDIA Corporation GA102 [GeForce RTX 3090] (rev a1)";
            rom = selfLib.file "GA102.rom.git-crypt";
          };
          gpu-sound = {
            id = "NVIDIA Corporation GA102 High Definition Audio Controller (rev a1)";
          };
        };
      };

      impermanence.enable = true;
      disko.devices.disk.main.device =
        "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_1TB_S4EWNF0M723324Z";
      disko.devices.disk.boot.device = "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0";

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
          llama-swap = pkgs.callPackage "${inputs.nixpkgs-unstable}/pkgs/by-name/ll/llama-swap/package.nix" {
            buildGoModule = pkgs.buildGo126Module;
          };
        };
      };

      environment.persistence."/persist".directories = [
        "/var/lib/private/llama-swap"
      ];

      system.activationScripts."createPersistentStorageDirs".deps = [
        "var-lib-private-permissions"
        "users"
        "groups"
      ];

      system.activationScripts = {
        "var-lib-private-permissions" = {
          deps = [ "specialfs" ];
          text = ''
            mkdir -p /persist/var/lib/private
            chmod 0700 /persist/var/lib/private
          '';
        };
      };

      systemd.services.llama-swap.serviceConfig.StateDirectory = "llama-swap";
      systemd.services.llama-swap.environment.HOME = "/var/lib/llama-swap";

      services.tailscale.serve.enable = true;
      services.tailscale.serve.configs.llama-swap = {
        target = "8080";
      };

      networking.hosts."${flakeConfig.inventory.ipAllocation.garage.home.primary.address}" = [
        "web.binarin.info"
      ];
      llama-models.models."Qwen3-Coder-30B-A3B-Instruct:Q4_K_S".url =
        "http://web.binarin.info:3902/files/Qwen3-Coder-30B-A3B-Instruct-Q4_K_S.gguf";
      llama-models.models."unsloth_Qwen3.5-9B-GGUF_Qwen3.5-9B-Q8_0".url =
        "http://web.binarin.info:3902/files/unsloth_Qwen3.5-9B-GGUF_Qwen3.5-9B-Q8_0.gguf";
      llama-models.models."gemma-4-26B-A4B-it:Q4_K_M".url =
        "http://web.binarin.info:3902/files/gemma-4-26B-A4B-it-UD-Q4_K_M.gguf";

      llama-models.configurations."gemma4" = {
        model = "gemma-4-26B-A4B-it:Q4_K_M";
        settings = {
          ctx-size = 131072;
          temp = 1.0;
          top-p = 0.95;
          top-k = 64;
        };
      };
      llama-models.configurations."qwen3-coder-30b" = {
        model = "Qwen3-Coder-30B-A3B-Instruct:Q4_K_S";
        settings = {
          ctx-size = 262144;
          temp = 0.7;
          top-p = 0.8;
          top-k = 20;
          repeat-penalty = 1.05;
        };
      };
      llama-models.configurations."qwen3.5-9b" = {
        model = "unsloth_Qwen3.5-9B-GGUF_Qwen3.5-9B-Q8_0";
        settings.ctx-size = 262144;
      };

      services.llama-swap = {
        enable = true;
        settings = {
          models =
            with lib;
            let
              llama-server = lib.getExe' pkgs.llama-cpp "llama-server";
              opts =
                s:
                with lib;
                pipe s [
                  (mapAttrsToList (opt: val: "--${opt} ${lib.escapeShellArg (builtins.toString val)}"))
                  (concatStringsSep " ")
                ];
            in
            flip mapAttrs config.llama-models.configurations (
              _: v: {
                cmd = ''
                  ${llama-server} -m ${
                    lib.escapeShellArg config.llama-models.models."${v.model}".path
                  } --port ''${PORT} ${opts v.settings}
                '';
              }
            );
        };
      };
    };
}
