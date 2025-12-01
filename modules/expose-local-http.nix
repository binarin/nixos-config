{ ... }:
{

  flake.nixosModules.expose-local-http =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    let
      cfg = config.services.caddy.expose-local-http;
    in
    {
      key = "nixos-config.modules.nixos.expose-local-http";

      options.services.caddy.expose-local-http = {
        enable = lib.mkEnableOption "Allow exposing local services over let's encrypted https";
        virtualHosts = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = { };
        };
      };

      config = lib.mkIf cfg.enable {
        sops.secrets.cloudflare-api-key = {
          sopsFile = "${config.lib.self.file' "secrets/webservers.yaml"}";
          restartUnits = [ "caddy.service" ];
        };

        systemd.services.caddy.serviceConfig.AmbientCapabilities = "CAP_NET_ADMIN CAP_NET_BIND_SERVICE";
        systemd.services.caddy.serviceConfig.LoadCredential =
          "cloudflare-api-token:${config.sops.secrets.cloudflare-api-key.path}";

        services.caddy = {
          enable = true;
          enableReload = false; # fails to reload when new hosts are added
          package = pkgs.caddy.withPlugins {
            plugins = [
              "github.com/caddy-dns/cloudflare@v0.0.0-20251022184029-2fc25ee62f40"
            ];
            hash = "sha256-sexPn0LzErmK8ptUICUPSSqLNLYIy7F9M3JBJfyCpJQ";
          };
          virtualHosts =
            with lib;
            flip mapAttrs cfg.virtualHosts (
              _hostname: backend: {
                extraConfig = ''
                  reverse_proxy ${backend}
                  tls {
                      dns cloudflare {file.{$CREDENTIALS_DIRECTORY}/cloudflare-api-token}
                      resolvers 1.1.1.1
                  }
                '';
              }
            );
        };
      };
    };
}
