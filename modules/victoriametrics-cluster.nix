{ ... }:
{
  # Single-host VictoriaMetrics cluster (vmstorage + vminsert + vmselect) for
  # producers that speak the multi-tenant /insert/<account>/... and
  # /select/<account>/... API. Fully separate from services.victoriametrics:
  # own ports, own storage.
  flake.nixosModules.victoriametrics-cluster =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.nixos-config.victoriametrics-cluster;

      package = pkgs.victoriametrics.overrideAttrs (old: rec {
        pname = "VictoriaMetrics-cluster";
        version = old.version;
        src = pkgs.fetchFromGitHub {
          owner = "VictoriaMetrics";
          repo = "VictoriaMetrics";
          tag = "v${version}-cluster";
          hash = "sha256-OY+tnLL4I9YOxBZT9vfuAuOeZWzGhmTWQIquiN9iREY=";
        };
        subPackages = [
          "app/vminsert"
          "app/vmselect"
          "app/vmstorage"
        ];
      });

      mkService =
        { description, args, extraServiceConfig ? { } }:
        {
          inherit description;
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig = {
            ExecStart = lib.escapeShellArgs args;
            DynamicUser = true;
            Restart = "on-failure";
            LimitNOFILE = 1048576;
            ProtectSystem = "strict";
            ProtectHome = true;
            PrivateTmp = true;
            NoNewPrivileges = true;
          }
          // extraServiceConfig;
        };
    in
    {
      key = "nixos-config.modules.nixos.victoriametrics-cluster";

      options.nixos-config.victoriametrics-cluster = {
        enable = lib.mkEnableOption "single-host VictoriaMetrics cluster";
        retentionPeriod = lib.mkOption {
          type = lib.types.str;
          default = "1";
          description = "vmstorage -retentionPeriod (bare number = months, or e.g. 30d, 1y).";
        };
        insertPort = lib.mkOption {
          type = lib.types.port;
          default = 8480;
        };
        selectPort = lib.mkOption {
          type = lib.types.port;
          default = 8481;
        };
        openFirewallOn = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "Interfaces on which vminsert/vmselect ports are opened.";
        };
      };

      config = lib.mkIf cfg.enable {
        environment.systemPackages = [ package ];

        systemd.services.vmstorage = mkService {
          description = "VictoriaMetrics cluster: vmstorage";
          args = [
            "${package}/bin/vmstorage"
            "-storageDataPath=/var/lib/vmstorage"
            "-retentionPeriod=${cfg.retentionPeriod}"
            "-httpListenAddr=127.0.0.1:8482"
            "-vminsertAddr=127.0.0.1:8400"
            "-vmselectAddr=127.0.0.1:8401"
          ];
          extraServiceConfig.StateDirectory = "vmstorage";
        };

        systemd.services.vminsert = mkService {
          description = "VictoriaMetrics cluster: vminsert";
          args = [
            "${package}/bin/vminsert"
            "-httpListenAddr=:${toString cfg.insertPort}"
            "-storageNode=127.0.0.1:8400"
          ];
        };

        systemd.services.vmselect = mkService {
          description = "VictoriaMetrics cluster: vmselect";
          args = [
            "${package}/bin/vmselect"
            "-httpListenAddr=:${toString cfg.selectPort}"
            "-storageNode=127.0.0.1:8401"
            "-cacheDataPath=/var/cache/vmselect"
          ];
          extraServiceConfig.CacheDirectory = "vmselect";
        };

        networking.firewall.interfaces = lib.genAttrs cfg.openFirewallOn (_: {
          allowedTCPPorts = [
            cfg.insertPort
            cfg.selectPort
          ];
        });
      };
    };
}
