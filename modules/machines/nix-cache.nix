{
  self,
  inputs,
  config,
  ...
}:
let
  flakeConfig = config;
in
{
  flake.nixosConfigurations.nix-cache = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.nix-cache-configuration
    ];

  };

  flake.nixosModules.nix-cache-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      nginx-cache-zone = "nix_cache_cache_zone";
    in
    {
      key = "nixos-config.nix-cache-configuration";
      imports = [
        self.nixosModules.default
        self.nixosModules.lxc
        self.nixosModules.impure-nix-setup
      ];

      config = {
        networking.hostName = "nix-cache";

        system.stateVersion = "24.11";

        nix.usePersonalNixCache = false; # we are the cache itself
        nix.settings.substituters = [ (lib.mkBefore "http://localhost?priority=10") ];

        sops.secrets.tailscale-auth = { };
        services.tailscale = {
          enable = true;
          authKeyFile = config.sops.secrets.tailscale-auth.path;
        };

        networking.firewall.allowedTCPPorts = [ 80 ];
        systemd.tmpfiles.rules = [ "Z- /cache 0755 nginx nginx -" ];
        systemd.services.nginx.serviceConfig.ReadWritePaths = [ "/cache" ];

        services.nginx.enable = true;

        services.nginx.appendHttpConfig = ''
          # Tell Nginx to set up a response cache at `/data/nginx/cache`, with a maximum
          # size of 800 GB or 400_000 stored files.
          # Also set `inactive` to tell Nginx to keep files until they haven't been
          # accessed for a year, instead of the default of removing files after they
          # haven't been accessed for 10 minutes.
          # See: https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_cache_path
          proxy_cache_path /cache max_size=800G keys_zone=${nginx-cache-zone}:50m inactive=365d;
        '';

        services.nginx.virtualHosts."cache-nixos-org.nix-cache" = {
          listen = [
            {
              port = 80;
              addr = "0.0.0.0";
            }
          ];

          serverAliases = [
            flakeConfig.inventory.ipAllocation."${config.networking.hostName}".home.primary.address
          ];

          locations."/" = {
            recommendedProxySettings = false;
            proxyPass = "https://cache.nixos.org";
            extraConfig = ''
              proxy_set_header Host $proxy_host;

              # Tell Nginx to actually use the response cache to cache requests.
              # See: https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_cache
              proxy_cache ${nginx-cache-zone};

              # Since Nix store paths are immutable, we can cache successful responses for a
              # long time.
              # We only want to cache successful responses: if we get a 404 error or a 401
              # error, we want the request to be retried the next time a client asks for it.
              # This is the default behaviour of `proxy_cache_valid` if no specific response
              # codes are specified.
              # See: https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_cache_valid
              proxy_cache_valid 200 365d;
              proxy_cache_use_stale error timeout invalid_header updating http_500 http_502 http_504 http_403 http_404 http_429;

              # Important: We need to ignore all common Cache-Control headers, since nginx by default
              # gives them HIGHER priority than the proxy_cache_valid directive above. We do not
              # want that, since we know that Nix urls are immutable.
              # See: https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_ignore_headers
              proxy_ignore_headers X-Accel-Expires Expires Cache-Control Set-Cookie;

              # Enable request deduplication for requests to the upstream servers.
              # This means that if two requests come in for the same store path the second
              # request will wait for the first request to complete (hopefully successfully)
              # and then serve that response, instead of opening two connections to the
              # upstream server.
              # See: https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_cache_lock
              proxy_cache_lock on;

              # Disable IPv6 resolution: our office network does not offer IPv6 addresses, but
              # Nginx will still attempt to connect to IPv6 addresses, which spams the error
              # log.
              # You'll probably want to remove this option if your network does support IPv6
              # or you use a different DNS server.
              # See: https://nginx.org/en/docs/http/ngx_http_core_module.html#resolver
              resolver ${lib.concatStringsSep " " config.inventory.networks.home.dns} ipv6=off;

              # When connecting to an upstream server, do use TLS SNI to indicate which server
              # to connect to. Without this option Nginx fails to connect to Cachix upstreams.
              # See: https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_ssl_server_name
              proxy_ssl_server_name on;

              # When connecting to an upstream server, do verify the TLS certificate since
              # this is outside of our network. Nginx defaults to not verifying certificates.
              # See: https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_ssl_trusted_certificate
              proxy_ssl_verify on;
              proxy_ssl_trusted_certificate /etc/ssl/certs/ca-certificates.crt;
            '';
          };
        };

        sops.secrets."nixos-config-runner-token" = {
          restartUnits = [
            ''gitea-runner-nixos\x2dconfig.service''
          ];
        };

        sops.templates.nixos-config-runner-token-env-file.content = ''
          TOKEN=${config.sops.placeholder."nixos-config-runner-token"}
        '';

        services.gitea-actions-runner = {
          package = pkgs.forgejo-runner;
          instances = {
            nixos-config = {
              enable = true;
              name = config.networking.hostName;
              url = "https://forgejo.lynx-lizard.ts.net";
              tokenFile = config.sops.templates.nixos-config-runner-token-env-file.path;
              labels = [ "native:host" ];
              hostPackages = with pkgs; [
                bash
                coreutils
                curl
                forgejo-cli
                gawk
                gitMinimal
                gnused
                jq
                just
                nix
                nodejs
                wget
              ];
            };
          };
        };

        services.nix-serve = {
          enable = true;
          port = 5000;
          openFirewall = true;
          secretKeyFile = "/var/lib/nix-cache/cache-priv-key.pem";
        };
      };
    };
}
