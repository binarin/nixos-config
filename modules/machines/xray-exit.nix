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
      v = config.clan.core.vars.generators;

      # tryEval tolerates eval before `clan vars generate`.
      secretPath = gen: file:
        let r = builtins.tryEval v.${gen}.files.${file}.path;
        in if r.success then r.value else "/run/secrets/${gen}/${file}";

      exitSkeleton = pkgs.writeText "xray-exit-skeleton.json" (builtins.toJSON (
        xrayLib.mkExitSettings {
          linkId = "@LINK@";
          exitDest = "@DEST@";
          exitSni = "@SNI@";
          exitPrivateKey = "@PK@";
          exitShortId = "@SID@";
        }
      ));

      assembler = pkgs.writeShellScript "xray-exit-assemble" ''
        set -euo pipefail
        out="$1"
        sed \
          -e "s|@LINK@|$(cat ${secretPath "xray-link" "uuid"})|g" \
          -e "s|@DEST@|$(cat ${secretPath "xray-exit-params" "dest"})|g" \
          -e "s|@SNI@|$(cat ${secretPath "xray-exit-params" "sni"})|g" \
          -e "s|@PK@|$(cat ${secretPath "xray-reality-exit" "private-key"})|g" \
          -e "s|@SID@|$(cat ${secretPath "xray-reality-exit" "short-id"})|g" \
          ${exitSkeleton} > "$out.tmp"
        mv "$out.tmp" "$out"
      '';
    in
    {
      key = "nixos-config.modules.nixos.xray-exit-configuration";

      imports = [
        self.nixosModules.nixos-base
        self.nixosModules.qemu-guest
        (selfLib.file' "machines/llm-runner/hardware-configuration.nix")

        # No sops: config is assembled from decrypted clan-var files (see
        # todo/xray-exit-clan-vars-no-sops.org), backend-neutral.
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

      systemd.services.xray-exit-assemble = {
        description = "Assemble xray.json from decrypted clan vars";
        wants = [ "sysinit.target" ];
        after = [ "sysinit.target" ];
        before = [ "xray.service" ];
        requiredBy = [ "xray.service" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${assembler} /run/xray-assembled/config.json";
          ExecStartPost = "${lib.getBin pkgs.systemd}/bin/systemctl try-restart xray.service";
          RemainAfterExit = true;
          RuntimeDirectory = "xray-assembled";
          RuntimeDirectoryMode = "0700";
        };
      };

      # Re-run the assembler on secret rotation; ExecStartPost then restarts xray.
      systemd.paths.xray-exit-assemble = {
        wantedBy = [ "multi-user.target" ];
        after = [ "sysinit.target" ];
        pathConfig = {
          Unit = "xray-exit-assemble.service";
          PathChanged = [
            (secretPath "xray-link" "uuid")
            (secretPath "xray-exit-params" "dest")
            (secretPath "xray-exit-params" "sni")
            (secretPath "xray-reality-exit" "private-key")
          ];
        };
      };

      systemd.services.xray = {
        requires = [ "xray-exit-assemble.service" ];
        after = [ "xray-exit-assemble.service" ];
      };

      services.xray = {
        enable = true;
        settingsFile = "/run/xray-assembled/config.json";
      };
    };
}
