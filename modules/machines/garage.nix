{
  self,
  inputs,
  config,
  ...
}:
let
  inventoryHostName = "garage";
  system = "x86_64-linux";
in
{
  flake.deploy.nodes.garage = {
    hostname = config.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.garage;
    };
  };

  flake.nixosConfigurations.garage = inputs.nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = {
      inherit inventoryHostName;
      flake = {
        inherit self inputs;
      };
    };
    modules = [
      self.nixosModules.garage-configuration
    ];
  };

  flake.nixosModules.garage-configuration =
    {
      config,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.garage-configuration";
      imports = [
        "${self}/machines/garage/hardware-configuration.nix"

        self.nixosModules.lxc
        self.nixosModules.baseline
        self.nixosModules.tailscale
        inputs.sops-nix.nixosModules.sops
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

        # Sops secrets
        sops.secrets."garage/rpc-secret" = { };
        sops.secrets."garage/admin-token" = { };
        sops.secrets."garage/metrics-token" = { };

        # Environment file with secrets for garage
        sops.templates."garage-env" = {
          content = ''
            GARAGE_RPC_SECRET=${config.sops.placeholder."garage/rpc-secret"}
            GARAGE_ADMIN_TOKEN=${config.sops.placeholder."garage/admin-token"}
            GARAGE_METRICS_TOKEN=${config.sops.placeholder."garage/metrics-token"}
          '';
          restartUnits = [ "garage.service" ];
        };

        services.garage = {
          enable = true;
          package = pkgs.garage;
          environmentFile = config.sops.templates."garage-env".path;
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
              root_domain = ".garage.lynx-lizard.ts.net";
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

        # Tailscale services
        services.tailscale.serve = {
          enable = true;
          services = {
            garage = {
              serviceName = "garage";
              protocol = "https";
              target = "localhost:3903"; # Admin API
            };
            s3 = {
              serviceName = "s3";
              protocol = "https";
              target = "localhost:3900"; # S3 API
            };
          };
        };
      };
    };
}
