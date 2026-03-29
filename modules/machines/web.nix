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
    };
}
