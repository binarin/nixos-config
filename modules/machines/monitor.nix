{
  self,
  config,
  lib,
  ...
}:
let
  flakeConfig = config;
in
{
  flake.deploy.nodes.monitor = {
    hostname = config.inventory.ipAllocation."monitor".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.monitor;
    };
  };

  clan.inventory.machines.monitor = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.monitor.home.primary.address;
  };

  clan.inventory.instances.postgres.roles.client.machines.monitor.settings.access.hledger-monitor-ro = {
    database = "hledger";
    user = "hledger_monitor_ro";
    role = "readonly";
    sourceCIDRs = [
      "100.64.0.0/10"
      "fd7a:115c:a1e0::/48"
    ];
    restartUnits = [ "grafana.service" ];
    secret.owner = "grafana";
  };

  clan.machines.monitor = {
    imports = [
      self.nixosModules.monitor-configuration
    ];
    nixpkgs.pkgs = self.lib.nixpkgs;
  };

  flake.nixosConfigurations.monitor = lib.mkForce (
    self.clan.nixosConfigurations.monitor.extendModules {
      specialArgs.inventoryHostName = "monitor";
    }
  );

  flake.nixosModules.monitor-configuration =
    {
      config,
      pkgs,
      ...
    }:
    let
      victoriaMetricsUid = "P4169E866C3094E38";

      # A Grafana-managed rule on VictoriaMetrics, one alert instance per series:
      # A queries, B takes each series' last value, C compares it with `threshold`.
      # Unset notification settings route it through the default policy.
      vmAlertRule =
        {
          uid,
          title,
          expr,
          threshold,
          for,
          summary,
          description,
        }:
        let
          lastTenMinutes = {
            from = 600;
            to = 0;
          };
          expression = {
            type = "__expr__";
            uid = "__expr__";
          };
        in
        {
          inherit uid title for;
          condition = "C";
          data = [
            {
              refId = "A";
              datasourceUid = victoriaMetricsUid;
              relativeTimeRange = lastTenMinutes;
              model = {
                refId = "A";
                datasource = {
                  type = "victoriametrics-metrics-datasource";
                  uid = victoriaMetricsUid;
                };
                inherit expr;
                editorMode = "code";
                range = true;
                instant = false;
                intervalMs = 60000;
                maxDataPoints = 43200;
              };
            }
            {
              refId = "B";
              datasourceUid = "__expr__";
              relativeTimeRange = lastTenMinutes;
              model = {
                refId = "B";
                datasource = expression;
                type = "reduce";
                expression = "A";
                reducer = "last";
              };
            }
            {
              refId = "C";
              datasourceUid = "__expr__";
              relativeTimeRange = lastTenMinutes;
              model = {
                refId = "C";
                datasource = expression;
                type = "threshold";
                expression = "B";
                conditions = [ { evaluator = threshold; } ];
              };
            }
          ];
          # A host whose series are gone (powered off, deleted) resolves rather
          # than alerting; the stale-probe rule covers a host that still reports.
          noDataState = "OK";
          execErrState = "Error";
          labels.severity = "warning";
          annotations = { inherit summary description; };
          isPaused = false;
        };
    in
    {
      key = "nixos-config.modules.nixos.monitor-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
        self.nixosModules.clan-smtp
        self.nixosModules.victoriametrics-cluster
      ];

      config = {
        networking.hostName = "monitor";
        nixos-config.export-metrics.enable = true;

        services.openssh.enable = true;
        services.openssh.settings.PermitRootLogin = "yes";

        environment.systemPackages = with pkgs; [ emacs-nox ];

        swanLake.sshTrust.accounts.root.directKeys = [ "binarin-at-nixos" ];

        nix.gc = {
          automatic = true;
          dates = "weekly";
          options = "--delete-older-than 30d";
        };

        services.victoriametrics = {
          enable = true;
          listenAddress = "0.0.0.0:8428";
        };

        networking.firewall.allowedTCPPorts = [
          8428
          3000
        ];

        # Multi-tenant store for Automind, separate from the single-node instance above.
        nixos-config.victoriametrics-cluster = {
          enable = true;
          retentionPeriod = "7d";
          openFirewallOn = [ config.services.tailscale.interfaceName ];
        };

        clan.core.vars.generators.grafana = {
          prompts.admin-username.description = "grafana admin username";
          prompts.admin-password.description = "grafana admin password";
          prompts.secret-key.description = "grafana secret key";
          files.admin-username = {
            secret = true;
            owner = config.users.users.grafana.name;
          };
          files.admin-password = {
            secret = true;
            owner = config.users.users.grafana.name;
          };
          files.secret-key = {
            secret = true;
            owner = config.users.users.grafana.name;
          };
          script = ''
            cat $prompts/admin-username > $out/admin-username
            cat $prompts/admin-password > $out/admin-password
            cat $prompts/secret-key     > $out/secret-key
          '';
        };

        # Let Grafana read the shared SMTP credentials (clan-smtp generator
        # publishes host/port/user/password with group smtp-password, 0640).
        users.users.grafana.extraGroups = [ "smtp-password" ];

        # Telegram bot token + chat id for the Grafana contact point.
        # Run `clan vars generate` on monitor to enter them, then deploy.
        clan.core.vars.generators.telegram = {
          prompts.token.description = "telegram bot token (from @BotFather)";
          prompts.chatid.description = "telegram chat id";
          files.token = {
            secret = true;
            owner = config.users.users.grafana.name;
          };
          files.chatid = {
            secret = true;
            owner = config.users.users.grafana.name;
          };
          script = ''
            cat $prompts/token  > $out/token
            cat $prompts/chatid > $out/chatid
          '';
        };

        services.grafana = {
          enable = true;
          declarativePlugins = with pkgs.grafanaPlugins; [ victoriametrics-metrics-datasource ];
          settings = {
            plugins = {
              # allow_loading_unsigned_plugins = "victoriametrics-datasource";
            };
            security = {
              admin_user = "$__file{${config.clan.core.vars.generators.grafana.files.admin-username.path}}";
              admin_password = "$__file{${config.clan.core.vars.generators.grafana.files.admin-password.path}}";
              secret_key = "$__file{${config.clan.core.vars.generators.grafana.files.secret-key.path}}";
            };
            smtp = {
              enabled = true;
              # $__file expands each occurrence, yielding "host:port".
              host = "$__file{${config.clan.core.vars.generators.smtp.files.host.path}}:$__file{${config.clan.core.vars.generators.smtp.files.port.path}}";
              user = "$__file{${config.clan.core.vars.generators.smtp.files.user.path}}";
              password = "$__file{${config.clan.core.vars.generators.smtp.files.password.path}}";
              from_address = "binarin@binarin.info";
              from_name = "Grafana (monitor)";
            };
          };
          provision = {
            enable = true;
            datasources.settings = {
              apiVersion = 1;
              datasources = [
                {
                  name = "VictoriaMetrics";
                  # The uid Grafana gave it when it was first provisioned, pinned so the
                  # provisioned alert rules can name it (the dashboards and
                  # monitoring/ use it too).
                  uid = victoriaMetricsUid;
                  type = "victoriametrics-metrics-datasource";
                  access = "proxy";
                  url = "http://127.0.0.1:8428";
                  isDefault = true;
                }
                {
                  # All tenants at once; filter with vm_account_id="<workspace>".
                  name = "Automind";
                  type = "victoriametrics-metrics-datasource";
                  access = "proxy";
                  url = "http://127.0.0.1:8481/select/multitenant/prometheus";
                }
                {
                  name = "hledger";
                  type = "postgres";
                  access = "proxy";
                  url = "postgres.lynx-lizard.ts.net:5432";
                  user = "hledger_monitor_ro";
                  jsonData = {
                    # Grafana's PostgreSQL datasource reads the DB name from
                    # jsonData.database; the top-level `database` field is ignored.
                    database = "hledger";
                    sslmode = "require";
                    postgresVersion = 1800;
                  };
                  secureJsonData.password =
                    "$__file{${config.clan.core.vars.generators."postgresql-postgres-hledger-hledger_monitor_ro".files.password.path}}";
                }
              ];
            };
            alerting = {
              contactPoints.settings = {
                apiVersion = 1;
                # Single contact point carrying both integrations, so every
                # routed alert notifies email AND telegram.
                contactPoints = [
                  {
                    orgId = 1;
                    name = "binarin-email";
                    receivers = [
                      {
                        uid = "binarin-email";
                        type = "email";
                        settings.addresses = "binarin@binarin.info";
                      }
                      {
                        uid = "binarin-telegram";
                        type = "telegram";
                        settings = {
                          bottoken = "$__file{${config.clan.core.vars.generators.telegram.files.token.path}}";
                          # Literal (not secret; useless without the token). Must be a
                          # quoted string — Grafana leaves $__file expansions unquoted,
                          # so a numeric chatid via $__file is parsed as a number and
                          # rejected ("cannot unmarshal number into ... chatid string").
                          chatid = "100771403";
                        };
                      }
                    ];
                  }
                ];
              };
              rules.settings = {
                apiVersion = 1;
                groups = [
                  {
                    orgId = 1;
                    name = "nix-cache";
                    folder = "infra";
                    interval = "1m";
                    rules = [
                      (vmAlertRule {
                        uid = "nix-cache-primary-down";
                        title = "nix cache primary upstream down";
                        # 1 while the probe gets the primary upstream's /nix-cache-info.
                        expr = ''max by (bhost) (nix_cache_upstream_ok{upstream="primary"})'';
                        threshold = {
                          type = "lt";
                          params = [ 1 ];
                        };
                        for = "30m";
                        summary = "{{ $labels.bhost }}: nix cache primary upstream down for 30m";
                        description = ''
                          The local nix cache proxy on {{ $labels.bhost }} gets no answer from its
                          primary upstream (garage on the home net, or ts.net when useHomeNet is
                          false); store path requests fall back to ts.net or fail with 502.
                          On the host: curl -i http://niks3-storage.nix-cache:48080/.upstream-nix-cache-info
                          and journalctl -u nginx -u nix-cache-upstream-probe (TLS errors, timeouts).
                          On garage: caddy and garage running and reachable from the host, and the
                          Tailscale cert of niks3-storage.lynx-lizard.ts.net that caddy gets from
                          tailscaled (journalctl -u caddy -u tailscaled).
                        '';
                      })
                      (vmAlertRule {
                        uid = "nix-cache-probe-stale";
                        title = "nix cache upstream probe stale";
                        # Seconds since the probe last wrote its textfile; it runs every 5m.
                        expr = "time() - max by (bhost) (nix_cache_upstream_probe_timestamp_seconds)";
                        threshold = {
                          type = "gt";
                          params = [ 1500 ];
                        };
                        for = "5m";
                        summary = "{{ $labels.bhost }}: nix cache upstream probe has not run for 30m";
                        description = ''
                          The nix cache upstream probe on {{ $labels.bhost }} last wrote its metric
                          {{ printf "%.0f" $values.B.Value }}s ago, so the nix cache primary alert
                          is blind there. On the host: systemctl status nix-cache-upstream-probe.timer
                          nix-cache-upstream-probe.service, and whether
                          /var/lib/prometheus-node-exporter-text-files/nix-cache-upstream.prom
                          is replaced every 5 minutes.
                        '';
                      })
                    ];
                  }
                ];
              };
              policies.settings = {
                apiVersion = 1;
                policies = [
                  {
                    orgId = 1;
                    receiver = "binarin-email";
                    group_by = [
                      "grafana_folder"
                      "alertname"
                    ];
                  }
                ];
              };
            };
          };
        };

        system.stateVersion = "24.05";
      };
    };
}
