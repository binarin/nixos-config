{ ... }:
{

  flake.nixosModules.use-nix-cache =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.use-nix-cache";

      options = {
        nix.usePersonalNixCache = lib.mkOption {
          type = lib.types.bool;
          default = true;
        };
      };

      config = lib.mkIf config.nix.usePersonalNixCache {
        services.nginx.enable = true;

        networking.hosts."127.0.0.1" = [
          "nix-cache-storage-lynx-lizard-ts-net.nix-cache"
        ];

        nix.settings.substituters = [
          (lib.mkBefore "http://nix-cache-storage-lynx-lizard-ts-net.nix-cache:48080?priority=8")
        ];

        nix.settings.trusted-public-keys = [
          "garage-store:Y7bsvHgog6rmkxL5rwZdua9cNBWfbAmWjbUVSasdIcY="
        ];

        services.nginx.virtualHosts."nix-cache-storage-lynx-lizard-ts-net.nix-cache" = {
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
            proxyPass = "https://nix-cache-storage.lynx-lizard.ts.net";
            extraConfig = ''
              # Use a very short timeout for connecting to the cache, since it should be available in the
              # local network.
              proxy_send_timeout 100ms;
              proxy_connect_timeout 100ms;

              # Serve a 404 response if the cache server cannot be reached:
              error_page 502 504 =404 @fallback;

              # Forward to the actual cache server:
              proxy_set_header Host nix-cache-storage.lynx-lizard.ts.net;
              proxy_ssl_name nix-cache-storage.lynx-lizard.ts.net;
              proxy_ssl_server_name on;
            '';
          };
        };
      };
    };
}
