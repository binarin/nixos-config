{ ... }:
{

  flake.nixosModules.use-nix-cache =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.use-nix-cache";

      options.nixos-config.personal-nix-cache.useHomeNet =
        with lib;
        mkOption {
          type = types.bool;
          default = true;
        };

      config = {
        services.nginx.enable = true;

        networking.hosts."127.0.0.1" = [
          "niks3-storage.nix-cache"
        ];

        nix.settings.substituters = [
          (lib.mkBefore "http://niks3-storage.nix-cache:48080?priority=8")
        ];

        nix.settings.trusted-public-keys = [
          "binarin-niks3-cache-1:m3JYEypT3iWb02SzKOgIwdqEyDLXk1XO4UTVYFTQWxM="
        ];

        services.nginx.virtualHosts."niks3-storage.nix-cache" = {
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
            proxyPass =
              if config.nixos-config.personal-nix-cache.useHomeNet then
                "https://niks3-storage.home.binarin.info"
              else
                "https://niks3-storage.lynx-lizard.ts.net";
            extraConfig = ''
              # Use a very short timeout for connecting to the cache, since it should be available in the
              # local network.
              proxy_send_timeout 2s;
              proxy_connect_timeout 2s;

              # Serve a 404 response if the cache server cannot be reached:
              error_page 502 504 =404 @fallback;

              # Forward to the actual cache server:
              proxy_set_header Host niks3-storaget.lynx-lizard.ts.net;
              proxy_ssl_name niks3-storage.lynx-lizard.ts.net;
              proxy_ssl_server_name on;
            '';
          };
        };
      };
    };
}
