{ self, inputs, ... }:
{
  flake.nixosConfigurations.furfur = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inventoryHostName = "furfur";
    };
    modules = [
      self.nixosModules.furfur-configuration
    ];
  };

  flake.nixosModules.furfur-configuration =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.furfur-configuration";

      imports = [
        self.nixosModules.baseline
        self.nixosModules.srvos-bits

        self.nixosModules.impermanence
        self.nixosModules.disko
        self.nixosModules.systemd-boot
        self.nixosModules.tailscale
        self.nixosModules.impure-nix-setup

        self.nixosModules.microsoft-surface

        self.nixosModules.kanata
        self.nixosModules.niri
        self.nixosModules.firefox
        self.nixosModules.bluetooth
        self.nixosModules.binarin-workstation
        self.nixosModules.binarin-podman
        self.nixosModules.binarin-nix-dev

        "${self}/machines/furfur/hardware-configuration.nix"
      ];

      users.users.binarin.extraGroups = [ "i2c" ];
      hardware.i2c.enable = true;

      services.avahi.enable = true;
      nixos-config.export-metrics.enable = false;
      home-manager.users.binarin = self.homeModules.furfur-binarin;
      environment.systemPackages = with pkgs; [
        zoom-us
      ];

      system.stateVersion = "25.11";
      hardware.microsoft-surface.kernelVersion = "stable";
      networking.hostName = "furfur";
      impermanence.enable = true;

      # services.kanata.keyboards.all.devices = [
      #   "/dev/input/by-path/platform-MSHW0263:00-event-kbd"
      # ];

      # Disable autostart for kanata - start manually with `systemctl start kanata-all`
      systemd.services.kanata-all.wantedBy = lib.mkForce [ ];

      services.logind.settings.Login = {
        HandlePowerKey = "suspend";
        HandlePowerKeyLongPress = "poweroff";

        HandleLidSwitch = "suspend";
        HandleLidSwitchExternalPower = "ignore";
        HandleLidSwitchDocked = "ignore";

        IdleAction = "ignore";
        IdleActionSec = "150";
      };

      boot.kernelParams = [
        "i915.enable_psr=0"
      ];

      boot.blacklistedKernelModules = [
        "intel_ipu6"
        "intel_ipu6_isys"
      ];

      fileSystems."/persist".neededForBoot = true;
      fileSystems."/local".neededForBoot = true;

      boot.initrd.luks.devices.luks1.crypttabExtraOpts = [
        "fido2-device=auto"
        "token-timeout=10s"
      ];

      systemd.tmpfiles.settings."10-persistent-ownership" = {
        "/persist/home/binarin".d = {
          user = "binarin";
          group = "binarin";
          mode = "0700";
        };
        "/local/home/binarin".d = {
          user = "binarin";
          group = "binarin";
          mode = "0700";
        };
        "/nix/var/nix/profiles/per-user/binarin".d = {
          user = "binarin";
          group = "binarin";
          mode = "0755";
        };
      };

      sops.secrets.binarin_password_hash.neededForUsers = true;
      users.users.binarin.hashedPasswordFile = config.sops.secrets.binarin_password_hash.path;

      sops.secrets.root_password_hash.neededForUsers = true;
      users.users.root.hashedPasswordFile = config.sops.secrets.root_password_hash.path;

      sops.secrets.agares_password = { };
      sops.templates.networkmanager-env-file.content = ''
        AGARES_PSK=${config.sops.placeholder.agares_password}
      '';

      time.hardwareClockInLocalTime = true;

      services.displayManager = {
        defaultSession = lib.mkForce "niri-uwsm";
      };

      programs.ssh.knownHosts = {
        furfur = {
          extraHostNames = [ ];
          publicKeyFile = config.lib.self.file' "secrets/furfur/ssh_host_ed25519_key.pub";
        };
      };

      networking.networkmanager.enable = true;
      networking.networkmanager.ensureProfiles = {
        environmentFiles = [
          config.sops.templates.networkmanager-env-file.path
        ];

        profiles = {
          agares = {
            connection = {
              id = "agares";
              type = "wifi";
            };
            ipv4 = {
              method = "auto";
            };
            ipv6 = {
              addr-gen-mode = "stable-privacy";
              method = "auto";
            };
            wifi = {
              mode = "infrastructure";
              ssid = "agares";
            };
            wifi-security = {
              key-mgmt = "wpa-psk";
              psk = "$AGARES_PSK";
            };
          };
        };
      };
    };

  flake.homeModules.furfur-binarin =
    { ... }:
    {
      key = "nixos-config.modules.home.furfur-binarin";

      programs.waybar.battery = {
        enable = true;
        name = "BAT1";
      };
    };

}
