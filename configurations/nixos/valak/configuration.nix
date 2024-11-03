# -*- nix -*-
{ flake, config, pkgs, lib, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    self.nixosModules.server
    self.nixosModules.bleeding
    self.nixosModules.emacs
    self.nixosModules.hyprland
    self.nixosModules.impure-nix-setup
    self.nixosModules.large-console-fonts
    (self + "/hardware/vfio.nix")
    (self + "/users/binarin.nix")
    (self + "/profile/workstation.nix")
    (self + "/packages/keep-flake-sources.nix")
  ];

  boot.kernelPackages = pkgs.linuxPackages_6_6;
  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "amdgpu" ];
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.allowHibernation = true;
  boot.zfs.forceImportRoot = false;
  boot.zfs.forceImportAll = false;

  boot.zfs.extraPools = [ "valak-vm-rpool" ];

  boot.extraModprobeConfig = ''
    options ddcci dyndbg
    options ddcci-backlight dyndbg
  '';

  virtualization.vfio = {
    enable = true;
    devices = [
      "0000:0a:00.0" # pci-e usb
      "0000:0e:00.0" # NVMe
      "0000:0f:00.0" # RTX
      "0000:0f:00.1"
    ];
  };

  hardware.bluetooth.disabledPlugins = [ "sap" ];

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

  hardware.i2c.enable = true;
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [
    "kvm-amd"
    "i2c_dev"
  ];
  boot.kernelParams = [ "amd_iommu=on" "iommu=pt" "pcie_aspm=off" "video=efifb:off" "pci=realloc" "usbcore.autosuspend=-1" ];
  boot.extraModulePackages = with config.boot.kernelPackages; [
    (ddcci-driver.overrideAttrs (oldAttrs: {
      patches = [ ./ddcci-quirk.patch ] ++ oldAttrs.patches;
    }))
  ];

  services.udev.extraRules = ''
    SUBSYSTEM=="i2c-dev", ACTION=="add",\
    ATTR{name}=="AMDGPU DM*",\
    TAG+="ddcci",\
    TAG+="systemd",\
    ENV{SYSTEMD_WANTS}+="ddcci@$kernel.service"
  '';

  systemd.services."ddcci@" =
    let
      script = pkgs.writers.writeBash "try-forcing-ddcci" ''
        dev=$1
        set -x
        echo Trying to attach ddcci to $dev
        success=0
        i=0
        id=$(echo $dev | cut -d "-" -f 2)
        while ((success < 1)) && ((i++ < 5)); do
          if ddcutil getvcp 10 -b $id; then
            success=1
            echo ddcci 0x37 > /sys/bus/i2c/devices/$dev/new_device
            echo "ddcci attached to %i"
          else
            sleep 5
          fi
        done
      '';
    in
    {
      path = with pkgs; [ bash ddcutil coreutils ];
      serviceConfig = {
        ExecStart = ''
          ${script} %i
        '';
      };
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

  # fileSystems."/" =
  #   { device = "valak-rpool/root/nixos";
  #     fsType = "zfs";
  #   };

  # fileSystems."/home" =
  #   { device = "valak-rpool/home";
  #     fsType = "zfs";
  #   };

  # fileSystems."/boot" =
  #   { device = "/dev/disk/by-label/VALAK-BOOT";
  #     fsType = "vfat";
  #   };

  # swapDevices =
  #   [ { device = "/dev/disk/by-label/valak-swap"; }
  #   ];

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/ac216764-ac4b-4a69-aa4e-22051798e14d";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/DC51-7F5A";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/27420222-5cdb-440f-9b32-0f2668db7d68"; }];

  nix.settings.max-jobs = lib.mkForce 8;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "valak"; # Define your hostname.
  networking.hostId = "55f9cd65";

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
      "40-br0" = {
        matchConfig.Name = "br0";

        address = [ "192.168.2.26/24" ];
        routes = [
          { routeConfig.Gateway = "192.168.2.1"; }
        ];
        dns = [ "192.168.2.46" "192.168.2.53" ];
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
    extraConfig = ''
      bind interfaces only = yes
      interfaces = lo smb-sketchup
    '';
    shares = {
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

  services.logind.extraConfig = ''
    HandlePowerKey=hibernate
  '';

  systemd.tmpfiles.rules = [
    "f /dev/shm/looking-glass 0660 binarin qemu-libvirtd -"
  ];

  system.stateVersion = "20.09";

  environment.systemPackages = with pkgs; [
    # syncoid works better with those
    lzop
    mbuffer

    (pkgs.writers.writeBashBin "brightnessctl-all"
      {
        makeWrapperArgs = [
          "--prefix"
          "PATH"
          ":"
          (lib.makeBinPath (with pkgs; [ brightnessctl coreutils ]))
        ];
      }
      ''
        mapfile -t all < <(brightnessctl -l -c backlight -m | cut -d , -f1)
        exit_code=0
        for dev in ''${all[@]} ; do
          if ! brightnessctl -d $dev "$@"; then
            exit_code=$?
          fi
        done
        exit $exit_code
      '')
  ];
}
