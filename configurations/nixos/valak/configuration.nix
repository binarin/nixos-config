# -*- nix -*-
{
  flake,
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  fileserverMounts = [
    "Music"
    "Movies"
    "Torrents"
  ];
in
{
  imports = [ "${self}/profile/workstation.nix" ];

  config = {
    console.useLargeFonts = true;

    # Use the systemd-boot EFI boot loader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.kernelPackages = pkgs.linuxPackages_6_6;
    boot.initrd.availableKernelModules = [
      "nvme"
      "xhci_pci"
      "ahci"
      "usbhid"
      "usb_storage"
      "sd_mod"
      "amdgpu"
    ];
    boot.supportedFilesystems = [ "zfs" ];
    boot.zfs.allowHibernation = true;
    boot.zfs.forceImportRoot = false;
    boot.zfs.forceImportAll = false;
    boot.zfs.extraPools = [ "valak-vm-rpool" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [
      "kvm-amd"
      "i2c_dev"
    ];
    boot.kernelParams = [
      "amd_iommu=on"
      "iommu=pt"
      "pcie_aspm=off"
      "video=efifb:off"
      "fbcon=map:1"
      "pci=realloc"
      "usbcore.autosuspend=-1"
    ];

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/ac216764-ac4b-4a69-aa4e-22051798e14d";
      fsType = "ext4";
    };

   fileSystems."/boot" = {
      device = "/dev/disk/by-uuid/DC51-7F5A";
      fsType = "vfat";
      options = ["umask=0077"];
    };

    swapDevices = [ { device = "/dev/disk/by-uuid/27420222-5cdb-440f-9b32-0f2668db7d68"; } ];

    hardware.enableAllFirmware = true;
    hardware.amdgpu.initrd.enable = true;
    hardware.amdgpu.opencl.enable = true;
    hardware.i2c.enable = true;
    hardware.bluetooth.disabledPlugins = [ "sap" ];

    virtualization.vfio = {
      enable = true;
      devices = [
        "0000:0a:00.0" # pci-e usb
        "0000:0e:00.0" # NVMe
        "0000:0f:00.0" # RTX
        "0000:0f:00.1"
      ];
    };

    virtualisation.libvirtd = {
      enable = true;
      qemu.ovmf.enable = true;
      qemu.ovmf.packages = [ pkgs.OVMFFull.fd ];
      qemu.runAsRoot = false;
      qemu.swtpm.enable = true;
      onBoot = "ignore";
      onShutdown = "shutdown";
    };

    systemd.services.libvirtd = {
      path =
        let
          env = pkgs.buildEnv {
            name = "qemu-hook-env";
            paths = with pkgs; [
              bash
              libvirt
              kmod
              systemd
            ];
          };
        in
        [ env ];
    };

    services.xserver = {
      deviceSection = ''
        BusID "PCI:18:0:0"
      '';
    };

    nix.settings.max-jobs = lib.mkForce 8;

    networking.useDHCP = false;

    systemd.network = {
      enable = true;
      links = {
        "20-eth25" = {
          matchConfig.MACAddress = "6c:1f:f7:15:cc:f7";
          linkConfig.Name = "eth25";
        };
      };
      netdevs = {
        "20-br0" = {
          netdevConfig = {
            Kind = "bridge";
            Name = "br0";
          };
        };
        "20-smb-sketchup" = {
          netdevConfig = {
            Kind = "bridge";
            Name = "smb-sketchup";
          };
        };
      };
      networks = {
        "30-eth25" = {
          matchConfig.Name = "eth25";
          networkConfig.Bridge = "br0";
          linkConfig.RequiredForOnline = "enslaved";
        };

        "40-br0" =
          let
            inherit (config.inventory.ipAllocation.valak.home.primary) addressWithPrefix;
            inherit (config.inventory.networks.home) gateway dns;
          in
          {
            matchConfig.Name = "br0";

            address = [ addressWithPrefix ];
            routes = [ { Gateway = gateway; } ];

            dns = dns;
            bridgeConfig = { };
            linkConfig = {
              # or "routable" with IP addresses configured
              RequiredForOnline = "routable";
            };
          };
        "40-smb-sketchup" = {
          matchConfig.Name = "smb-sketchup";
          address = [ "172.16.242.2/24" ];
          networkConfig.ConfigureWithoutCarrier = "yes";
        };
      };
    };
    services.samba = {
      enable = true;
      settings = {
        global = {
          "bind interfaces only" = "yes";
          "interfaces" = "lo smb-sketchup";
        };
        public = {
          path = "/home/vm-shared/current";
          "guest ok" = "yes";
          writeable = "yes";
          public = "yes";
          "hosts allow" = "172.16.242.*";
        };
      };
    };

    # port 1688 is for KMS server for windows
    networking.firewall.extraCommands = ''
      iptables -A INPUT -i smb-sketchup -p tcp -m multiport --dports 139,445,1688 -j ACCEPT || true
      iptables -A INPUT -i smb-sketchup -p udp -m multiport --dports 137,138 -j ACCEPT || true
      iptables -A INPUT -i virbr0 -p tcp -m multiport --dports 24800 -j ACCEPT || true
      iptables -A FORWARD -i tailscale+ -o smb-sketchup -j ACCEPT || true
      iptables -A FORWARD -i smb-sketchup -o tailscale+ -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT || true
      iptables -t nat -A POSTROUTING -s 100.64.0.0/10 -o smb-sketchup -j MASQUERADE || true
    '';

    networking.firewall.extraStopCommands = ''
      iptables -D INPUT -i smb-sketchup -p tcp -m multiport --dports 139,445,1688 -j ACCEPT || true
      iptables -D INPUT -i smb-sketchup -p udp -m multiport --dports 137,138 -j ACCEPT || true
      iptables -D INPUT -i virbr0 -p tcp -m multiport --dports 24800 -j ACCEPT || true
      iptables -D FORWARD -i tailscale+ -o smb-sketchup -j ACCEPT || true
      iptables -D FORWARD -i smb-sketchup -o tailscale+ -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT || true
      iptables -t nat -D POSTROUTING -s 100.64.0.0/10 -o smb-sketchup -j MASQUERADE || true
    '';

    # '--exclude-prefix=/dev' is everywhere, kick off looking-glass manually
    system.activationScripts.looking-glass = ''
      ${pkgs.systemd}/bin/systemd-tmpfiles --create --prefix=/dev/shm/
    '';

    services.logind.powerKey = "hibernate";

    systemd.tmpfiles.rules = [
      "f /dev/shm/looking-glass 0660 binarin qemu-libvirtd -"
      "d- /var/lib/servarr 02775 binarin servarr -"
      "Z- /var/lib/servarr 02775 binarin servarr -"
    ];

    environment.systemPackages = with pkgs; [
      # syncoid works better with those
      lzop
      mbuffer
      brightnessctl-all
    ];

    specialisation.headless.configuration = {
      virtualization.vfio = {
        devices = [
          "0000:12:00.0" # Radeon
          "0000:12:00.1"
          "0000:12:00.2"
          "0000:12:00.3"
        ];
      };
    };

    sops.secrets."fileserver-samba/username" = { };
    sops.secrets."fileserver-samba/password" = { };
    sops.templates.fileserver-samba-credentials.content = ''
      username=${config.sops.placeholder."fileserver-samba/username"}
      password=${config.sops.placeholder."fileserver-samba/password"}
    '';
    systemd.mounts = lib.flip map fileserverMounts (mnt: {
      what = "//192.168.2.79/${mnt}";
      where = "/mnt/${mnt}";
      type = "cifs";
      options = "rw,uid=binarin,cred=${config.sops.templates.fileserver-samba-credentials.path}";
    });

    systemd.automounts = lib.flip map fileserverMounts (mnt: {
      where = "/mnt/${mnt}";
      automountConfig = {
        TimeoutIdleSec = "600";
      };
    });
    services.sabnzbd.enable = true;
    services.radarr.enable = true;
    services.prowlarr.enable = true;
    users.users.sabnzbd.extraGroups = [ "servarr" ];
    users.users.radarr.extraGroups = [ "servarr" ];
    users.groups.servarr = {};
    networking.firewall.allowedTCPPorts = [ 443 ];


    sops.secrets.xray-config = {
      format = "binary";
      sopsFile = "${config.lib.self.file' "secrets/valak/xray-json.bin"}";
    };

    services.xray = {
      enable = true;
      settingsFile = config.sops.secrets.xray-config.path;
    };
  };
}
