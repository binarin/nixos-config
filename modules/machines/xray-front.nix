{
  self,
  lib,
  inputs,
  ...
}:
let
  xrayLib = import "${self}/lib/xray/config-lib.nix";
  # The nixpkgs EC2 image-builder module lives under `nixos/maintainers/`,
  # not `nixos/modules/`, so it's not reachable via `modulesPath`; and
  # referencing `pkgs.path` inside the NixOS module causes infinite recursion
  # (pkgs is derived from this eval). Resolve it here in the flake scope.
  amazonImageModule = "${inputs.nixpkgs}/nixos/maintainers/scripts/ec2/amazon-image.nix";
in
{
  # NOTE: no `flake.deploy.nodes.xray-front` and no static `deploy.targetHost`
  # (the EC2 public address is not committed). Deploy with an explicit target:
  #   clan machines update xray-front --target-host root@<ec2-public-dns>

  clan.machines.xray-front = {
    imports = [ self.nixosModules.xray-front-configuration ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosConfigurations.xray-front = lib.mkForce (
    self.clan.nixosConfigurations.xray-front.extendModules {
      specialArgs.inventoryHostName = "xray-front";
    }
  );

  flake.nixosModules.xray-front-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      # ph/val tolerate eval before `clan vars generate` (tryEval fallback) — same as xray-exit.
      ph =
        name:
        let
          r = builtins.tryEval config.sops.placeholder."vars/${name}";
        in
        if r.success then r.value else "<SOPS:PLACEHOLDER:${name}>";
      val =
        g: f:
        let
          r = builtins.tryEval (config.clan.core.vars.generators.${g}.files.${f}.value or "");
        in
        if r.success then r.value else "";

      # Client skeleton: fully generic (only structure + @SENTINEL@ tokens).
      # The per-user generator seds real values in from its uuid + dependencies.
      clientSkeleton = pkgs.writeText "xray-client-skeleton.json" (
        builtins.toJSON (
          xrayLib.mkClientSettings {
            userId = "@UUID@";
            frontEndpoint = "@FRONT_ENDPOINT@";
            frontPort = "@FRONT_PORT@";
            frontSni = "@FRONT_SNI@";
            frontPublicKey = "@FRONT_PBK@";
            frontShortId = "@FRONT_SID@";
            bypassGeosite = "@BYPASS_GEOSITE@";
            bypassGeoip = "@BYPASS_GEOIP@";
          }
        )
      );

      mkUserGenerator = userName: {
        name = "xray-user-${userName}";
        value = {
          files.uuid = {
            secret = true;
            deploy = true;
            restartUnits = [ "xray.service" ];
          };
          # client.json is a client artifact — never deployed to a server.
          files."client.json" = {
            secret = true;
            deploy = false;
          };
          dependencies = [
            "xray-front-params"
            "xray-reality-front"
            "xray-geo"
          ];
          runtimeInputs = [
            pkgs.xray
            pkgs.gnused
            pkgs.coreutils
          ];
          script = ''
            xray uuid | tr -d '\n' > "$out/uuid"
            sed \
              -e "s|@UUID@|$(cat "$out/uuid")|g" \
              -e "s|@FRONT_ENDPOINT@|$(cat "$in/xray-front-params/endpoint")|g" \
              -e "s|@FRONT_PORT@|$(cat "$in/xray-front-params/port")|g" \
              -e "s|@FRONT_SNI@|$(cat "$in/xray-front-params/sni")|g" \
              -e "s|@FRONT_PBK@|$(cat "$in/xray-reality-front/public-key")|g" \
              -e "s|@FRONT_SID@|$(cat "$in/xray-reality-front/short-id")|g" \
              -e "s|@BYPASS_GEOSITE@|$(cat "$in/xray-geo/bypass-geosite")|g" \
              -e "s|@BYPASS_GEOIP@|$(cat "$in/xray-geo/bypass-geoip")|g" \
              ${clientSkeleton} > "$out/client.json"
          '';
        };
      };
    in
    {
      key = "nixos-config.modules.nixos.xray-front-configuration";
      imports = [
        self.nixosModules.nixos-base
        self.nixosModules.systemd-boot
        self.nixosModules.sops
        # The nixpkgs EC2 image-builder module: defines
        # `system.build.amazonImage` (VHD via make-disk-image.nix with
        # partitionTableType "efi"), wires the nvme/ena/xen kernel modules,
        # ttyS0 console, growPartition, fetch-ec2-metadata, etc. This
        # replaces the hand-rolled EC2 block we used to carry AND the
        # my-machines/xray-front/disko.nix layout (make-disk-image owns the
        # GPT/ESP/ext4 layout itself; disko's runtime activation would
        # wrongly re-partition an AMI-booted EBS volume).
        #
        # NB: this module lives under `nixos/maintainers/`, not
        # `nixos/modules/`, so it's not reachable via `modulesPath`.
        amazonImageModule
        self.nixosModules.xray-shared
      ];

      # x86_64 EC2, but we boot UEFI instances (the AMI's partition table is
      # GPT/ESP). amazon-image.nix derives `partitionTableType` and the
      # bootloader wiring from `config.ec2.efi`, which defaults to
      # isAarch64 — so set it explicitly.
      ec2.efi = true;

      # amazon-image.nix sets `networking.hostName = mkDefault ""` (so EC2
      # metadata can supply it at boot); clan sets it to the inventory machine
      # name at the same priority, causing a conflict. We want the clan value
      # (the machine *is* xray-front), so force it.
      networking.hostName = lib.mkForce "xray-front";

      # Same clan-openssh/sshd (tags.all) pre-`clan vars generate` issue as xray-exit:
      # the file-backed knownHosts value would be forced before generation. mkForce empties it.
      programs.ssh.knownHosts = lib.mkForce { };

      # amazon-image.nix already sets boot.loader.grub for UEFI and the
      # nvme/ena/xen modules + ttyS0 console + growPartition; nothing
      # EC2-specific to add here.

      # EC2 gives an address over DHCP; use systemd-networkd on the primary NIC.
      systemd.network.enable = true;
      networking.useDHCP = false;
      systemd.network.networks."10-ec2" = {
        matchConfig.Name = "en*";
        networkConfig.DHCP = "yes";
      };

      networking.firewall.allowedTCPPorts = [
        22
        443
      ];

      # Clan owns the SSH host key (decrypted from a vars generator into
      # /run/secrets/vars/openssh/...) and root's authorized_keys (via the
      # `sshd` service instance in nixos-base). nixpkgs' EC2 metadata
      # services that do the same job from IMDS / user-data are therefore
      # redundant here — disable them so the system's secret model has
      # exactly one source of truth and no dead /etc/ssh/ssh_host_*_key
      # files lying around. (Hostname still comes from EC2 via networking.hostName
      # = "" default if we ever want it; we set it explicitly via clan/inventory.)
      systemd.services.apply-ec2-data.enable = false;
      systemd.services.fetch-ec2-metadata.enable = lib.mkForce false;

      # # --- Per-user credential + client-config generators ---
      # clan.core.vars.generators = builtins.listToAttrs (map mkUserGenerator xrayLib.userNames);

      # # --- Front server config: inbound :443 with all user UUIDs, chaining to exit ---
      # sops.templates."xray.json" = {
      #   restartUnits = [ "xray.service" ];
      #   content = builtins.toJSON (xrayLib.mkFrontSettings {
      #     userIds = map (n: ph "xray-user-${n}/uuid") xrayLib.userNames;
      #     linkId = ph "xray-link/uuid";
      #     frontDest = ph "xray-front-params/dest";
      #     frontSni = ph "xray-front-params/sni";
      #     frontPrivateKey = ph "xray-reality-front/private-key";
      #     frontShortId = val "xray-reality-front" "short-id";
      #     exitEndpoint = ph "xray-exit-params/endpoint";
      #     exitPort = ph "xray-exit-params/port";
      #     exitSni = ph "xray-exit-params/sni";
      #     exitPublicKey = val "xray-reality-exit" "public-key";
      #     exitShortId = val "xray-reality-exit" "short-id";
      #   });
      # };

      # services.xray = {
      #   enable = true;
      #   settingsFile = config.sops.templates."xray.json".path;
      # };
      # system.stateVersion is managed by clanCore's `state-version` generator
      # (do not set it here — an explicit mkDefault collides with clanCore's).
    };
}
