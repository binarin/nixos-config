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
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.furfur-configuration";

      imports = [
        self.nixosModules.baseline

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
        self.nixosModules.user-binarin

        "${self}/machines/furfur/hardware-configuration.nix"
      ];

      home-manager.users.binarin = self.homeModules.furfur-binarin;

      system.stateVersion = "25.11";
      hardware.microsoft-surface.kernelVersion = "stable";
      networking.hostName = "furfur";
      impermanence.enable = true;

      services.kanata.keyboards.all.devices = [
        "/dev/input/by-path/platform-MSHW0263:00-event-kbd"
      ];

      # Buggy, it get's another short press immediately after wake-up if woken up with the same key.
      services.logind.settings.Login.HandlePowerKey = "ignore";
      services.logind.settings.Login.HandlePowerKeyLongPress = "suspend";

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

      services.swayidle.binarin.isLaptop = true;

      programs.waybar.battery = {
        enable = true;
        name = "BAT1";
      };
    };

}
