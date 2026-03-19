{
  self,
  inputs,
  ...
}:
let
  # WiFi credentials from environment variables (only available in impure mode)
  wifiSsid = builtins.getEnv "WIFI_SSID";
  wifiPassword = builtins.getEnv "WIFI_PASSWORD";
  hasWifiCredentials = wifiSsid != "" && wifiPassword != "";
in
{
  flake.nixosConfigurations.iso = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs.inventoryHostName = "iso";
    modules = [
      self.nixosModules.iso-configuration
    ];
  };

  flake.nixosModules.iso-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.iso-configuration";
      imports = [
        "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
        "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"

        self.nixosModules.baseline
        self.nixosModules.microsoft-surface
        self.nixosModules.sshd
        self.nixosModules.kanata
        self.nixosModules.large-console-fonts
      ];

      config = {
        nixos-config.export-metrics.enable = false;

        # build faster
        isoImage.squashfsCompression = "gzip -Xcompression-level 1";

        networking.hostName = "iso";

        nixpkgs.hostPlatform = "x86_64-linux";

        # NOTE: Don't bump without re-installing complete system!
        system.stateVersion = "25.11";

        boot = {
          supportedFilesystems = lib.mkForce [
            "btrfs"
            "reiserfs"
            "vfat"
            "f2fs"
            "xfs"
            "ntfs"
            "cifs"
            "zfs"
            "exfat"
          ];
          kernelParams = [
            "console=tty0"
            "console=ttyS0,115200n8"
            # Use eth0 naming to match Proxmox cloud-init network config
            "net.ifnames=0"
          ];
        };

        # Serial console login
        systemd.services."serial-getty@ttyS0".enable = true;

        services.openssh.enable = true;
        services.openssh.settings.PermitRootLogin = lib.mkForce "yes";
        services.openssh.authorizedKeysInHomedir = lib.mkForce true;

        services.tailscale.enable = true;

        services.cloud-init.enable = true;
        services.cloud-init.network.enable = true;
        # Allow network config on all boot types (cloud-init 25.x uses boot-legacy)
        services.cloud-init.settings.updates.network.when = [
          "boot"
          "boot-legacy"
        ];

        # Networkd DHCP fallback for eth0 (overridden by cloud-init static config)
        systemd.network.networks."20-eth0-dhcp" = {
          matchConfig.Name = "eth0";
          networkConfig.DHCP = "yes";
        };

        environment.systemPackages = with pkgs; [
          inputs.disko.packages."${config.nixpkgs.hostPlatform.system}".disko
        ];

        users.users.nixos.password = "nixos";
        users.users.nixos.initialHashedPassword = lib.mkForce null;
        users.users.nixos.openssh.authorizedPrincipals = [
          "root"
          "binarin"
        ];

        users.users.root.openssh.authorizedPrincipals = [
          "root"
          "binarin"
        ];

        networking.networkmanager.enable = true;
        # Let networkd handle eth0 (cloud-init static IP), NM handles WiFi
        networking.networkmanager.unmanaged = [ "interface-name:eth0" ];

        # WiFi credentials injected via environment variables in impure mode
        # In pure mode (CI, regular eval), no WiFi credentials are configured
        environment.etc = lib.mkIf hasWifiCredentials {
          "NetworkManager/system-connections/${wifiSsid}.nmconnection" = {
            mode = "0600";
            text = ''
              [connection]
              id=${wifiSsid}
              type=wifi

              [wifi]
              mode=infrastructure
              ssid=${wifiSsid}

              [wifi-security]
              key-mgmt=wpa-psk
              psk=${wifiPassword}

              [ipv4]
              method=auto

              [ipv6]
              addr-gen-mode=stable-privacy
              method=auto
            '';
          };
        };

        systemd = {
          targets = {
            sleep.enable = false;
            suspend.enable = false;
            hibernate.enable = false;
            hybrid-sleep.enable = false;
          };
        };
      };
    };
}
