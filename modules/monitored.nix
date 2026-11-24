{ ... }:
{
  flake.nixosModules.monitored =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.monitored";
      options.nixos-config.export-metrics = {
        enable = lib.mkOption {
          description = "Enabled sending metrics to centralized victoriametrics";
          type = lib.types.bool;
          # no default, force the user to choose
        };
        textfileDirectory = lib.mkOption {
          description = ''
            Directory read by node exporter's textfile collector. A producer writes
            its own `<name>.prom` there atomically (temp file in the same directory,
            then rename), running with `textfileGroup` as a supplementary group.
          '';
          type = lib.types.str;
          default = "/var/lib/prometheus-node-exporter-text-files";
          readOnly = true;
        };
        textfileGroup = lib.mkOption {
          description = "Group allowed to write into `textfileDirectory`.";
          type = lib.types.str;
          default = "node-exporter-textfile";
          readOnly = true;
        };
      };
      config =
        let
          cfg = config.nixos-config.export-metrics;
        in
        {
          services.prometheus.exporters.node = {
            enable = cfg.enable;
            port = 9100;
            extraFlags = [
              "--collector.filesystem.fs-types-exclude='^(tmpfs|proc|sysfs|ramfs)'"
            ]
            # In a CT /sys/class/hwmon belongs to the host, so every guest reports the
            # same sensors as if they were its own. On valak that also means each guest
            # hammers the host's Aquacomputer OCTO, which needs ~7s per attribute read
            # while holding the driver mutex, wedging scrapes in uninterruptible sleep
            # until they hit --web.max-requests. Only furfur and qdevice are real
            # hardware, and both keep the collector.
            ++ lib.optional config.boot.isContainer "--no-collector.hwmon"
            ++ [ "--collector.textfile.directory=${cfg.textfileDirectory}" ];
            # enabledCollectors = [
            #   "logind"
            #   "systemd"
            # ];
            # disabledCollectors = [ "textfile" ];
            # openFirewall = true;
            # firewallFilter = "-i br0 -p tcp -m tcp --dport 9100";
          };

          users.groups = lib.mkIf cfg.enable { ${cfg.textfileGroup} = { }; };

          # setgid, so every producer's file is readable under the same group.
          systemd.tmpfiles.settings.node-exporter-textfile = lib.mkIf cfg.enable {
            ${cfg.textfileDirectory}.d = {
              mode = "2775";
              user = "root";
              group = cfg.textfileGroup;
            };
          };

          services.vmagent = {
            enable = cfg.enable;
            remoteWrite.url = "http://192.168.2.2:8428/api/v1/write";
            # openFirewall = true;
            prometheusConfig = {
              global = {
                external_labels = {
                  bhost = "${config.networking.hostName}";
                };
              };
              scrape_configs = [
                {
                  job_name = "node";
                  scrape_interval = "10s";
                  static_configs = [
                    {
                      targets = [ "127.0.0.1:9100" ];
                    }
                  ];
                }
              ];
            };
          };
        };
    };
}
