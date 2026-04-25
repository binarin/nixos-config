{
  self,
  config,
  lib,
  inputs,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
in
{
  flake.deploy.nodes.web = {
    hostname = "web";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.web;
    };
  };

  clan.inventory.machines.web = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.web.home.primary.address;
  };

  clan.machines.web = {
    imports = [
      self.nixosModules.web-configuration
      "${inputs.nixpkgs-unstable}/nixos/modules/services/web-apps/librechat.nix"
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
  };

  clan.inventory.instances.acme = {
    roles.client.machines.web = {
      settings = {
        domain = "web.clan.binarin.info";
        extraDomainNames = [
          "binarin.info"
          "web.home.binarin.info"
          "unifi.home.binarin.info"
          "unifi.clan.binarin.info"
          "unifi.binarin.info"
          "librechat.binarin.info"
        ];
      };
    };
  };

  flake.nixosConfigurations.web = lib.mkForce (
    self.clan.nixosConfigurations.web.extendModules {
      specialArgs.inventoryHostName = "web";
    }
  );

  flake.nixosModules.web-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.web-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
      ];

      nixos-config.export-metrics.enable = false;

      services.nginx.enable = true;
      networking.firewall.allowedTCPPorts = [
        80
        443
      ];

      services.nginx.virtualHosts."unifi.binarin.info" = {
        addSSL = true;
        sslCertificate = "/var/lib/ssl-cert/full.pem";
        sslCertificateKey = "/var/lib/ssl-cert/full.pem";
        serverAliases = [
          "unifi.home.binarin.info"
          "unifi.clan.binarin.info"
        ];

        locations."/api/ws/" = {
          proxyPass = "https://usg.home.binarin.info:443";
          extraConfig = ''
            proxy_set_header        Host $host;
            proxy_set_header        X-Real-IP $remote_addr;
            proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header        X-Forwarded-Proto $scheme;
            proxy_set_header        X-Forwarded-Host $host;
            proxy_set_header        X-Forwarded-Server $hostname;
            proxy_ssl_verify       off;
            proxy_ssl_protocols TLSv1 TLSv1.1 TLSv1.2 TLSv1.3;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
          '';
        };

        locations."/" = {
          proxyPass = "https://usg.home.binarin.info:443";
          extraConfig = ''
            proxy_set_header        Host $host;
            proxy_set_header        X-Real-IP $remote_addr;
            proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header        X-Forwarded-Proto $scheme;
            proxy_set_header        X-Forwarded-Host $host;
            proxy_set_header        X-Forwarded-Server $hostname;
            proxy_ssl_verify       off;
            proxy_ssl_protocols TLSv1 TLSv1.1 TLSv1.2 TLSv1.3;
            proxy_http_version 1.1;
          '';
        };
      };

      services.nginx.virtualHosts."binarin.info" = {
        addSSL = true;
        sslCertificate = "/var/lib/ssl-cert/full.pem";
        sslCertificateKey = "/var/lib/ssl-cert/full.pem";

        locations."~ ^(/_matrix/|/.well-known/matrix/)" = {
          priority = 100;
          proxyPass = "http://127.0.0.1:6167$request_uri";
          extraConfig = ''
            proxy_set_header        Host $host;
            proxy_set_header        X-Real-IP $remote_addr;
            proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header        X-Forwarded-Proto $scheme;
            proxy_set_header        X-Forwarded-Host $host;
            proxy_set_header        X-Forwarded-Server $hostname;
          '';
        };

        locations."/" = {
          proxyPass = "https://social.clan.binarin.info/";
          extraConfig = ''
            proxy_set_header        Host $host;
            proxy_set_header        X-Real-IP $remote_addr;
            proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header        X-Forwarded-Proto $scheme;
            proxy_set_header        X-Forwarded-Host $host;
            proxy_set_header        X-Forwarded-Server $hostname;
          '';
        };

        locations."/files/" = {
          proxyPass = "http://${flakeConfig.inventory.ipAllocation.garage.home.primary.address}:3902$request_uri";
          extraConfig = ''
            proxy_set_header Host web.binarin.info;
          '';
        };
      };

      clan.core.vars.generators.cloudflare-tunnel = {
        prompts.credentials-json.description = "credentials json file";
        files.credentials-json = { };
        script = ''
          cat $prompts/credentials-json > $out/credentials-json
        '';
      };

      services.matrix-continuwuity = {
        enable = true;
        settings = {
          global.server_name = "binarin.info";
          global.address = [ "127.0.0.1" ];
          global.well_known.client = "https://binarin.info";
          global.well_known.server = "binarin.info:443";
          global.new_user_displayname_suffix = "";
        };
      };

      services.cloudflared = {
        enable = true;
        tunnels = {
          "fa87c2c5-0c46-4334-9278-0f0ac72c7caa" = {
            credentialsFile = config.clan.core.vars.generators.cloudflare-tunnel.files.credentials-json.path;
            ingress = {
              "binarin.info" = {
                service = "http://localhost:80";
              };
            };
            default = "http_status:404";
          };
        };
      };

      clan.core.vars.generators.librechat = {
        files.creds-key = { };
        files.creds-iv = { };
        files.jwt-secret = { };
        files.jwt-refresh-secret = { };
        runtimeInputs = [ pkgs.openssl ];
        script = ''
          openssl rand -hex 32 > $out/creds-key
          openssl rand -hex 16 > $out/creds-iv
          openssl rand -hex 64 > $out/jwt-secret
          openssl rand -hex 64 > $out/jwt-refresh-secret
        '';
      };

      services.librechat = {
        enable = true;
        package = inputs.nixpkgs-unstable.legacyPackages."${pkgs.stdenv.hostPlatform.system}".librechat;
        credentials = {
          CREDS_KEY = config.clan.core.vars.generators.librechat.files.creds-key.path;
          CREDS_IV = config.clan.core.vars.generators.librechat.files.creds-iv.path;
          JWT_SECRET = config.clan.core.vars.generators.librechat.files.jwt-secret.path;
          JWT_REFRESH_SECRET = config.clan.core.vars.generators.librechat.files.jwt-refresh-secret.path;
        };
        enableLocalDB = true;
        env = {
          PORT = 3080;
          DEBUG_LOGGING = "true";

        };
        settings = {
          version = "1.3.0";
          registration = {
            allowedDomains = [ "binarin.info" ];
          };
          endpoints = {
            custom = [
              {
                name = "llm-runner";
                baseURL = "https://llm-runner.lynx-lizard.ts.net/v1";
                models = {
                  default = [ "gemma4" ];
                };
                apiKey = "no";
                titleConvo = true;
                titleModel = "current_model";
                modelDisplayLabel = "llm-runner";
              }
            ];
          };
        };
      };

      services.nginx.virtualHosts."librechat.binarin.info" = {
        sslCertificate = "/var/lib/ssl-cert/full.pem";
        sslCertificateKey = "/var/lib/ssl-cert/full.pem";
        locations."/" = {
          proxyPass = "http://localhost:${config.services.librechat.env.PORT}";
        };
      };

    };
}
