{
  self,
  inputs,
  config,
  lib,
  ...
}:
let
  inventoryHostName = "garage";
  flakeConfig = config;
in
{
  flake.deploy.nodes.garage = {
    hostname = config.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.garage;
    };
  };

  clan.inventory.machines.garage = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.${inventoryHostName}.home.primary.address;
  };

  clan.machines.garage = {
    imports = [
      self.nixosModules.garage-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosConfigurations.garage = lib.mkForce (
    self.clan.nixosConfigurations.garage.extendModules {
      specialArgs.inventoryHostName = inventoryHostName;
    }
  );

  flake.nixosModules.garage-configuration =
    {
      config,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.garage-configuration";
      imports = [
        "${self}/my-machines/garage/hardware-configuration.nix"

        self.nixosModules.lxc
        self.nixosModules.baseline
        self.nixosModules.tailscale
        inputs.sops-nix.nixosModules.sops
        self.nixosModules.expose-local-http
      ];

      config = {
        networking.hostName = inventoryHostName;
        system.stateVersion = "25.11";
        nixos-config.export-metrics.enable = false;

        proxmoxLXC.mounts = [
          {
            # /nix mount - uses default pool (local-zfs)
            mountPoint = "/nix";
            size = "32G";
          }
          {
            # garage data mount - on spinning-zfs, 1TB
            pool = "spinning-zfs";
            mountPoint = "/var/lib/garage/data";
            size = "1T";
          }
        ];

        # Secrets: raw values (imported 1:1 via `clan vars set`) composed into
        # an env-file for garage.service. One generator per secret keeps each
        # importable; garage-env depends on them and renders the env fragment.
        clan.core.vars.generators.garage-rpc-secret = {
          prompts.rpc-secret.description = "Garage RPC secret";
          files.rpc-secret = { };
          script = ''
            cat $prompts/rpc-secret > $out/rpc-secret
          '';
        };
        clan.core.vars.generators.garage-admin-token = {
          prompts.admin-token.description = "Garage admin API token";
          files.admin-token = { };
          script = ''
            cat $prompts/admin-token > $out/admin-token
          '';
        };
        clan.core.vars.generators.garage-metrics-token = {
          prompts.metrics-token.description = "Garage metrics token";
          files.metrics-token = { };
          script = ''
            cat $prompts/metrics-token > $out/metrics-token
          '';
        };
        clan.core.vars.generators.garage-env = {
          dependencies = [
            "garage-rpc-secret"
            "garage-admin-token"
            "garage-metrics-token"
          ];
          files.garage-env = { };
          script = ''
            {
              printf 'GARAGE_RPC_SECRET=%s\n' "$(cat $in/garage-rpc-secret/rpc-secret)"
              printf 'GARAGE_ADMIN_TOKEN=%s\n' "$(cat $in/garage-admin-token/admin-token)"
              printf 'GARAGE_METRICS_TOKEN=%s\n' "$(cat $in/garage-metrics-token/metrics-token)"
            } > $out/garage-env
          '';
        };

        services.garage = {
          enable = true;
          package = pkgs.garage;
          environmentFile = config.clan.core.vars.generators.garage-env.files.garage-env.path;
          settings = {
            metadata_dir = "/var/lib/garage/meta";
            data_dir = "/var/lib/garage/data";
            db_engine = "sqlite";

            replication_factor = 1;

            rpc_bind_addr = "[::]:3901";
            rpc_public_addr = "127.0.0.1:3901";
            # rpc_secret injected via GARAGE_RPC_SECRET env var

            s3_api = {
              s3_region = "garage";
              api_bind_addr = "[::]:3900";
              root_domain = ".s3.lynx-lizard.ts.net";
            };

            s3_web = {
              bind_addr = "[::]:3902";
              root_domain = ".lynx-lizard.ts.net";
              index = "index.html";
            };

            k2v_api = {
              api_bind_addr = "[::]:3904";
            };

            admin = {
              api_bind_addr = "[::]:3903";
              # admin_token and metrics_token injected via env vars
            };
          };
        };

        services.caddy.expose-local-http.enable = true;
        services.caddy.virtualHosts."niks3-storage.home.binarin.info".extraConfig = ''
          reverse_proxy http://127.0.0.1:3902 {
                    header_up Host niks3-storage.lynx-lizard.ts.net
          }
          import letsencrypt
        '';
        services.caddy.virtualHosts."http://niks3-storage.home.binarin.info".extraConfig = ''
          reverse_proxy http://127.0.0.1:3902 {
                    header_up Host niks3-storage.lynx-lizard.ts.net
          }
        '';
        networking.firewall.allowedTCPPorts = [
          80
          443
          3902
        ];

        # Tailscale services
        services.tailscale.serve = {
          enable = true;
          services = {
            garage = {
              serviceName = "garage-api";
              protocol = "https";
              target = "localhost:3903"; # Admin API
            };
            s3 = {
              serviceName = "s3";
              protocol = "https";
              target = "localhost:3900"; # S3 API
            };
            niks3-storage = {
              serviceName = "niks3-storage";
              protocol = "https";
              target = "localhost:3902";
            };
            forgejo-artifacts = {
              serviceName = "forgejo-artifacts";
              protocol = "https";
              target = "localhost:3902";
            };
          };
        };
      };
    };
}
