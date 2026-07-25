{
  self,
  inputs,
  config,
  lib,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
in
{
  flake.deploy.nodes.docker-on-nixos = {
    hostname = config.inventory.ipAllocation."docker-on-nixos".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.docker-on-nixos;
    };
  };

  clan.inventory.machines.docker-on-nixos = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.docker-on-nixos.home.primary.address;
  };

  clan.machines.docker-on-nixos = {
    imports = [
      self.nixosModules.docker-on-nixos-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosConfigurations.docker-on-nixos = lib.mkForce (
    self.clan.nixosConfigurations.docker-on-nixos.extendModules {
      specialArgs.inventoryHostName = "docker-on-nixos";
    }
  );

  clan.inventory.instances.acme = {
    roles.client.machines.docker-on-nixos = {
      settings = {
        domain = "docker-on-nixos.clan.binarin.info";
        extraDomainNames = [
          "homebox.binarin.info"
          "karakeep.binarin.info"
          "archivebox.binarin.info"
        ];
      };
    };
  };

  flake.nixosModules.docker-on-nixos-configuration =
    {
      config,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.docker-on-nixos-configuration";

      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
        self.nixosModules.impermanence
        "${inputs.nixpkgs}/nixos/modules/profiles/minimal.nix"
        inputs.arion.nixosModules.arion

        self.nixosModules.homebox
        self.nixosModules.karakeep
        self.nixosModules.nativelink
        self.nixosModules.archivebox
      ];

      config = {
        networking.hostName = "docker-on-nixos";

        impermanence.enable = true;
        system.stateVersion = "24.11";

        proxmoxLXC = {
          cores = 8;
          memory = 16384;
          rootfs.size = "8G";
          mounts = lib.mkForce [
            {
              mountPoint = "/sbin";
              size = "1G";
              backup = true;
            }
            {
              mountPoint = "/nix";
              size = "32G";
              backup = true;
            }
            {
              mountPoint = "/persist";
              size = "64G";
              backup = true;
            }
            {
              mountPoint = "/local";
              size = "1G";
              backup = false;
            }
            {
              mountPoint = "/mnt/archivebox/data";
              size = "1T";
              pool = "spinning-zfs";
              backup = true;
            }
            {
              mountPoint = "/mnt/archivebox/var";
              size = "10G";
              backup = true;
            }
            {
              mountPoint = "/mnt/karakeep/var";
              size = "8G";
              backup = true;
            }
            {
              mountPoint = "/mnt/karakeep/data";
              size = "1T";
              pool = "spinning-zfs";
              backup = true;
            }
          ];
        };

        virtualisation.docker.enable = true;
        virtualisation.docker.autoPrune.enable = true;
        virtualisation.arion.backend = "docker";

        environment.persistence."/persist".directories = [
          "/var/lib/docker"
        ];

        # nginx reverse proxy with clan ACME certs (replaces caddy + cloudflare)
        services.nginx.enable = true;
        # Bind to explicit addresses, NOT the nixpkgs default 0.0.0.0/[::0]. A
        # wildcard :443 bind swallows the :443 of the tailscale vip-services that
        # share this node (e.g. bazel-cache -> nativelink), which have their own
        # advertised IPs. Binding specific addresses leaves those vip IPs free.
        #
        # LAN address serves these vhosts on the local net; the node's own
        # tailscale IPs serve them over the tailnet, since their public DNS
        # (homebox/brick-tracker.binarin.info) points at this node's tailscale
        # IP and the clan ACME cert already covers those names.
        services.nginx.defaultListenAddresses = [
          flakeConfig.inventory.ipAllocation.docker-on-nixos.home.primary.address
          "100.86.235.105" # node tailscale IPv4 (docker-on-nixos.lynx-lizard.ts.net)
          "[fd7a:115c:a1e0::5401:eb69]" # node tailscale IPv6 (bracketed for listen)
        ];
        networking.firewall.allowedTCPPorts = [
          80
          443
        ];

        services.nginx.virtualHosts."homebox.binarin.info" = {
          addSSL = true;
          sslCertificate = "/var/lib/ssl-cert/full.pem";
          sslCertificateKey = "/var/lib/ssl-cert/full.pem";
          locations."/" = {
            proxyPass = "http://localhost:7745";
            # homebox derives its auth cookie's Domain from the Host header it
            # receives (Domain: noPort(r.Host)). Without forwarding the real Host,
            # nginx's default sends `Host: localhost`, so the browser rejects the
            # `hb.auth.token` cookie as "invalid domain" and every request 401s
            # ("authorization header or query is required") -> silent login loop.
            recommendedProxySettings = true; # proxy_set_header Host $host; + X-Forwarded-*
          };
        };

        services.nginx.virtualHosts."karakeep.binarin.info" = {
          addSSL = true;
          sslCertificate = "/var/lib/ssl-cert/full.pem";
          sslCertificateKey = "/var/lib/ssl-cert/full.pem";
          locations."/" = {
            proxyPass = "http://localhost:3000";
          };
        };

        services.nginx.virtualHosts."archivebox.binarin.info" = {
          addSSL = true;
          sslCertificate = "/var/lib/ssl-cert/full.pem";
          sslCertificateKey = "/var/lib/ssl-cert/full.pem";
          locations."/" = {
            proxyPass = "http://localhost:8000";
          };
        };

        environment.systemPackages = with pkgs; [ litecli ];

        nixos-config.export-metrics.enable = true;
      };
    };
}
