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
            ];
            # enabledCollectors = [
            #   "logind"
            #   "systemd"
            # ];
            # disabledCollectors = [ "textfile" ];
            # openFirewall = true;
            # firewallFilter = "-i br0 -p tcp -m tcp --dport 9100";
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
