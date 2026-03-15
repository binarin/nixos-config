{ self, config, ... }:
let
  selfLib = self.lib.self;
in
{

  flake.nixosModules.expose-local-http-no-secret =
    {
      lib,
      specialArgs,
      config,
      pkgs,
      ...
    }:
    let
      cfg = config.services.caddy.expose-local-http;
    in
    {
      key = "nixos-config.modules.nixos.expose-local-http-no-secret";

      options.services.caddy.expose-local-http = {
        enable = lib.mkEnableOption "Allow exposing local services over let's encrypted https";
        virtualHosts = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = { };
        };
      };

      config = lib.mkIf cfg.enable {

        systemd.services.caddy.serviceConfig.AmbientCapabilities = "CAP_NET_ADMIN CAP_NET_BIND_SERVICE";

        services.caddy = {
          enable = true;
          enableReload = false; # fails to reload when new hosts are added
          package = self.packages."${pkgs.stdenv.hostPlatform.system}".caddy-with-cloudflare-dns;
          extraConfig = ''
            (letsencrypt) {
              tls {
                  dns cloudflare {file.{$CREDENTIALS_DIRECTORY}/cloudflare-api-token}
                  propagation_delay 300s
                  propagation_timeout 1800s
                  dns_ttl 60s
                  resolvers sri.ns.cloudflare.com vera.ns.cloudflare.com
              }
            }
          '';
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

  flake.nixosModules.expose-local-http-clan =
    {
      config,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.expose-local-http-clan";
      imports = [ self.nixosModules.expose-local-http-no-secret ];
      clan.core.vars.generators.cloudflare-acme = {
        share = true;
        prompts.api-token.description = "cloudflare api token for dns-01 acme challenge";
        files.api-token = { };
        script = ''
          cat $prompts/api-token > $out/api-token
        '';
      };
      systemd.services.caddy.serviceConfig.LoadCredential =
        lib.mkForce "cloudflare-api-token:${config.clan.core.vars.generators.cloudflare-acme.files.api-token.path}";
    };

  flake.nixosModules.expose-local-http =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.expose-local-http";
      imports = [ self.nixosModules.expose-local-http-no-secret ];
      sops.secrets.cloudflare-api-key = {
        sopsFile = "${selfLib.file' "secrets/webservers.yaml"}";
        restartUnits = [ "caddy.service" ];
      };

      systemd.services.caddy.serviceConfig.LoadCredential =
        "cloudflare-api-token:${config.sops.secrets.cloudflare-api-key.path}";

    };
}
