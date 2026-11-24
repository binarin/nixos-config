{ ... }:
{

  flake.nixosModules.use-nix-cache =
    { config, lib, ... }:
    let
      cfg = config.nixos-config.personal-nix-cache;

      tsnetHost = "niks3-storage.lynx-lizard.ts.net";
      homeNetUrl = "https://niks3-storage.home.binarin.info";

      # Names the upstream that produced the response: primary, tsnet or none.
      upstreamHeader = "X-Nix-Cache-Upstream";

      # Both upstreams are reached with the ts.net name: garage's caddy serves it on
      # the LAN address too.
      upstreamNameConfig = ''
        proxy_set_header Host ${tsnetHost};
        proxy_ssl_name ${tsnetHost};
        proxy_ssl_server_name on;
      '';

      # garage on the local network. A failure here (502/504, whether nginx's own or
      # sent by garage's caddy) is retried at ts.net by @tsnet. proxy_intercept_errors
      # only intercepts codes that have an error_page, so a real 404 passes through.
      # recursive_error_pages lets @tsnet's own error_page apply after this one.
      homeNetProxyConfig = ''
        proxy_pass ${homeNetUrl};
        ${upstreamNameConfig}
        # Short timeouts: garage is on the local network, and the ts.net retry
        # should start quickly when it cannot be reached.
        proxy_connect_timeout 2s;
        proxy_send_timeout 2s;
        add_header ${upstreamHeader} primary always;
        proxy_intercept_errors on;
        recursive_error_pages on;
        error_page 502 504 = @tsnet;
      '';

      # The ts.net name is resolved per request through tailscaled's MagicDNS (hence
      # the variable in proxy_pass), so nginx still starts while tailscaled is down.
      # A 502/504 here, or a failed lookup, becomes the 502 of @nix_cache_unavailable.
      tsnetProxyConfig = upstreamRole: ''
        resolver 100.100.100.100 ipv6=off valid=60s;
        resolver_timeout 3s;
        set $niks3_tsnet_url https://${tsnetHost};
        proxy_pass $niks3_tsnet_url;
        ${upstreamNameConfig}
        # A fresh tailnet path (DERP or direct) may take a few seconds to set up.
        proxy_connect_timeout 5s;
        proxy_send_timeout 5s;
        add_header ${upstreamHeader} ${upstreamRole} always;
        proxy_intercept_errors on;
        error_page 502 504 =502 @nix_cache_unavailable;
      '';

      primaryProxyConfig = if cfg.useHomeNet then homeNetProxyConfig else tsnetProxyConfig "primary";

      # rewrite ... break ends the rewrite-module directives of a location, so it
      # comes after the `set` of tsnetProxyConfig.
      upstreamCacheInfo = ''
        rewrite ^ /nix-cache-info break;
      '';

    in
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
          locations = {
            "/nix-cache-info".return = ''
              200 "StoreDir: /nix/store\nWantMassQuery: 1\nPriority: 41\n"
            '';

            # Every store path request: primary upstream, then ts.net; 502 when both
            # fail, so nix reports a broken substituter instead of a silent miss.
            "/".extraConfig = primaryProxyConfig;

            # Probe: upstream's own /nix-cache-info through the same primary-then-ts.net
            # path; X-Nix-Cache-Upstream says which one answered.
            "= /.upstream-nix-cache-info".extraConfig = primaryProxyConfig + upstreamCacheInfo;

            # Probe: ts.net alone, so the fallback is checked while the primary works.
            "= /.upstream-nix-cache-info/tsnet".extraConfig = tsnetProxyConfig "tsnet" + upstreamCacheInfo;

            "@nix_cache_unavailable".extraConfig = ''
              add_header ${upstreamHeader} none always;
              return 502 "no nix cache upstream answered\n";
            '';
          }
          // lib.optionalAttrs cfg.useHomeNet {
            "@tsnet".extraConfig = tsnetProxyConfig "tsnet";
          };
        };
      };
    };
}
