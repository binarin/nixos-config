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
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
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
    {
      key = "nixos-config.modules.nixos.monitor-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
      ];

      config = {
        networking.hostName = "monitor";
        nixos-config.export-metrics.enable = true;

        services.openssh.enable = true;
        services.openssh.settings.PermitRootLogin = "yes";

        environment.systemPackages = with pkgs; [ emacs-nox ];

        users.users."root".openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMCVAKqmUdCkJ1gbi2ZA6vLnmf880U/9v5bfxhChapWB binarin@nixos"
        ];

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
          };
          provision = {
            enable = true;
            datasources.settings = {
              apiVersion = 1;
              datasources = [
                {
                  name = "VictoriaMetrics";
                  type = "victoriametrics-metrics-datasource";
                  access = "proxy";
                  url = "http://127.0.0.1:8428";
                  isDefault = true;
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
          };
        };

        system.stateVersion = "24.05";
      };
    };
}
