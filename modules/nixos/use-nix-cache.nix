{
  flake,
  config,
  pkgs,
  lib,
  ...
}:
{
  options = {
    nix.usePersonalNixCache = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf config.nix.usePersonalNixCache {
    services.nginx.enable = true;

    networking.hosts."127.0.0.1" = [
      "cache-nixos-org.nix-cache"
      "nix-cache-lynx-lizard-ts-net.nix-cache"
      "nix-cache-home-binarin-info.nix-cache"
    ];

    nix.settings.substituters = [
      (lib.mkBefore "http://nix-cache-home-binarin-info.nix-cache:48080?priority=9")
      (lib.mkBefore "http://nix-cache-lynx-lizard-ts-net.nix-cache:48080?priority=10")
      (lib.mkBefore "http://cache-nixos-org.nix-cache:48080?priority=11")
    ];

    nix.settings.trusted-public-keys = [
      "nix-cache.lynx-lizard.ts.net:KQHX8UgajbQqfKNjG0m5Hw44anrXkZ6NaHvaUU0ey6g="
    ];

    services.nginx.virtualHosts."cache-nixos-org.nix-cache" = {
      listen = [
        {
          addr = "127.0.0.1";
          port = 48080;
        }
      ];
      locations."/nix-cache-info".return = ''
        200 "StoreDir: /nix/store\nWantMassQuery: 1\nPriority: 41\n"
      '';
      locations."@fallback".return = ''
        200 "404"
      '';
      locations."/" = {
        proxyPass = "http://nix-cache.home.binarin.info";
        extraConfig = ''
          # Use a very short timeout for connecting to the cache, since it should be available in the
          # local network.
          proxy_send_timeout 100ms;
          proxy_connect_timeout 100ms;

          # Serve a 404 response if the cache server cannot be reached:
          error_page 502 504 =404 @fallback;

          # Forward to the actual cache server:
          proxy_set_header Host $host;
          proxy_set_header Authorization $http_authorization;
        '';
      };
    };

    services.nginx.virtualHosts."nix-cache-lynx-lizard-ts-net.nix-cache" = {
      listen = [
        {
          addr = "127.0.0.1";
          port = 48080;
        }
      ];
      locations."/nix-cache-info".return = ''
        200 "StoreDir: /nix/store\nWantMassQuery: 1\nPriority: 41\n"
      '';
      locations."@fallback".return = ''
        200 "404"
      '';
      locations."/" = {
        proxyPass = "http://100.83.194.104:5000"; # XXX
        extraConfig = ''
          # Use a very short timeout for connecting to the cache, since it should be available in the
          # local network.
          proxy_send_timeout 100ms;
          proxy_connect_timeout 100ms;

          # Serve a 404 response if the cache server cannot be reached:
          error_page 502 504 =404 @fallback;

          # Forward to the actual cache server:
          proxy_set_header Host $host;
          proxy_set_header Authorization $http_authorization;
          proxy_ssl_name nix-cache.lynx-lizard.ts.net;
          proxy_ssl_server_name on;
        '';
      };
    };

    services.nginx.virtualHosts."nix-cache-home-binarin-info.nix-cache" = {
      listen = [
        {
          addr = "127.0.0.1";
          port = 48080;
        }
      ];
      locations."/nix-cache-info".return = ''
        200 "StoreDir: /nix/store\nWantMassQuery: 1\nPriority: 41\n"
      '';
      locations."@fallback".return = ''
        200 "404"
      '';
      locations."/" = {
        proxyPass = "http://nix-cache.home.binarin.info:5000";
        extraConfig = ''
          # Use a very short timeout for connecting to the cache, since it should be available in the
          # local network.
          proxy_send_timeout 100ms;
          proxy_connect_timeout 100ms;

          # Serve a 404 response if the cache server cannot be reached:
          error_page 502 504 =404 @fallback;

          # Forward to the actual cache server:
          proxy_set_header Host $host;
          proxy_set_header Authorization $http_authorization;
          proxy_ssl_name nix-cache.lynx-lizard.ts.net;
          proxy_ssl_server_name on;
        '';
      };
    };
  };
}
