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
    let
      # ph: get sops placeholder, or a safe fallback when the secret hasn't
      # been registered yet (clan vars generate not run).
      ph = name:
        let r = builtins.tryEval config.sops.placeholder."vars/${name}";
        in if r.success then r.value else "<SOPS:PLACEHOLDER:${name}>";
      val = g: f:
        let r = builtins.tryEval (config.clan.core.vars.generators.${g}.files.${f}.value or "");
        in if r.success then r.value else "";
    in
    {
      key = "nixos-config.modules.nixos.xray-exit-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.qemu-guest
        self.nixosModules.disko-template-zfs-whole
        self.nixosModules.disko-template-add-uefi-boot-to-main
        self.nixosModules.xray-shared
        (selfLib.file' "machines/llm-runner/hardware-configuration.nix")
      ];

      # No Tailscale on this machine.
      services.tailscale.enable = lib.mkForce false;
      nixos-config.export-metrics.enable = false;

      # Bypass clan openssh generator requirement. The binarin-admin
      # sshd instance uses tags.all, which would trigger the openssh
      # generator's file-backed value before clan vars generate has run.
      # lib.mkForce prevents the sshd perInstance definition from
      # being forced.
      programs.ssh.knownHosts = lib.mkForce { };

      # Guest-VLAN networking: override qemu-guest's home-VLAN 40-qemu block.
      systemd.network.networks."40-qemu" = lib.mkForce {
        matchConfig.Name = "eth0";
        dns = flakeConfig.inventory.networks.guest.dns;
        address = [
          flakeConfig.inventory.ipAllocation.xray-exit.guest.primary.addressWithPrefix
        ];
        routes = [ { Gateway = flakeConfig.inventory.networks.guest.gateway; } ];
      };

      disko.devices.disk.main.device = "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0";
      nixos-config.qemu-guest.proxmox = {
        memory = 1024;
        balloon = 512;
        cores = 2;
        bios = "ovmf";
        machine = "q35";
        description = "xray-exit (residential VPN exit)";
        disks = [
          {
            type = "image";
            storage = "local-zfs";
            size = "16G";
            bootOrder = 1;
          }
        ];
      };

      # Public REALITY inbound reachable via the router port-forward.
      networking.firewall.allowedTCPPorts = [ 8443 ];

      sops.templates."xray.json" = {
        restartUnits = [ "xray.service" ];
        content = builtins.toJSON (xrayLib.mkExitSettings {
          linkId = ph "xray-link/uuid";
          exitDest = ph "xray-exit-params/dest";
          exitSni = ph "xray-exit-params/sni";
          exitPrivateKey = ph "xray-reality-exit/private-key";
          exitShortId = val "xray-reality-exit" "short-id";
        });
      };

      services.xray = {
        enable = true;
        settingsFile = config.sops.templates."xray.json".path;
      };
    };
}
