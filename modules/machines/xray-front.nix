{
  self,
  lib,
  ...
}:
let
  xrayLib = import "${self}/lib/xray/config-lib.nix";
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
      ph = name:
        let r = builtins.tryEval config.sops.placeholder."vars/${name}";
        in if r.success then r.value else "<SOPS:PLACEHOLDER:${name}>";
      val = g: f:
        let r = builtins.tryEval (config.clan.core.vars.generators.${g}.files.${f}.value or "");
        in if r.success then r.value else "";

      # Client skeleton: fully generic (only structure + @SENTINEL@ tokens).
      # The per-user generator seds real values in from its uuid + dependencies.
      clientSkeleton = pkgs.writeText "xray-client-skeleton.json" (builtins.toJSON (
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
      ));

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
          dependencies = [ "xray-front-params" "xray-reality-front" "xray-geo" ];
          runtimeInputs = [ pkgs.xray pkgs.gnused pkgs.coreutils ];
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
        self.nixosModules.baseline
        self.nixosModules.systemd-boot
        # Do NOT use self.nixosModules.disko here: it reads specialArgs.inventoryHostName
        # to locate my-machines/<host>/disko.nix, but that specialArg is injected only by
        # the flake-output extendModules wrapper — it is ABSENT when clan evaluates the
        # machine for `clan vars` (and this machine declares clan-var generators), so the
        # module throws. Import the layout file by explicit path instead; disko activation
        # comes from clan-core's global disko module (same as acme / llm-runner, which use
        # disko without self.nixosModules.disko).
        "${self}/my-machines/xray-front/disko.nix"
        self.nixosModules.xray-shared
      ];

      # No Tailscale on this machine.
      services.tailscale.enable = lib.mkForce false;
      nixos-config.export-metrics.enable = false;

      # Same clan-openssh/sshd (tags.all) pre-`clan vars generate` issue as xray-exit:
      # the file-backed knownHosts value would be forced before generation. mkForce empties it.
      programs.ssh.knownHosts = lib.mkForce { };

      # --- EC2 hardware/boot (UEFI). Launch the instance in UEFI boot mode. ---
      boot.loader.efi.canTouchEfiVariables = lib.mkForce false;
      boot.initrd.availableKernelModules = [ "nvme" "ena" "xen_blkfront" ];
      boot.growPartition = true;
      boot.kernelParams = [ "console=ttyS0,115200n8" ];

      # EC2 gives an address over DHCP; use systemd-networkd on the primary NIC.
      systemd.network.enable = true;
      networking.useDHCP = false;
      systemd.network.networks."10-ec2" = {
        matchConfig.Name = "en*";
        networkConfig.DHCP = "yes";
      };

      networking.firewall.allowedTCPPorts = [ 22 443 ];

      # --- Per-user credential + client-config generators ---
      clan.core.vars.generators = builtins.listToAttrs (map mkUserGenerator xrayLib.userNames);

      # --- Front server config: inbound :443 with all user UUIDs, chaining to exit ---
      sops.templates."xray.json" = {
        restartUnits = [ "xray.service" ];
        content = builtins.toJSON (xrayLib.mkFrontSettings {
          userIds = map (n: ph "xray-user-${n}/uuid") xrayLib.userNames;
          linkId = ph "xray-link/uuid";
          frontDest = ph "xray-front-params/dest";
          frontSni = ph "xray-front-params/sni";
          frontPrivateKey = ph "xray-reality-front/private-key";
          frontShortId = val "xray-reality-front" "short-id";
          exitEndpoint = ph "xray-exit-params/endpoint";
          exitPort = ph "xray-exit-params/port";
          exitSni = ph "xray-exit-params/sni";
          exitPublicKey = val "xray-reality-exit" "public-key";
          exitShortId = val "xray-reality-exit" "short-id";
        });
      };

      services.xray = {
        enable = true;
        settingsFile = config.sops.templates."xray.json".path;
      };
      # system.stateVersion is managed by clanCore's `state-version` generator
      # (do not set it here — an explicit mkDefault collides with clanCore's).
    };
}
