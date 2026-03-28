{ self, ... }:
{
  flake.clan.modules.lets-encrypt =
    { lib, config, ... }:
    let
      clanConfig = config;
    in
    {
      _class = "clan.service";
      manifest.name = "nixos-config-let-encrypt";
      manifest.readme = ''
        centrally issued and distributed let's encrypt certs (via dns-01 challenge)
      '';
      roles.server = {
        description = "will issue certs and make age-encrypted certs available via https for other machines to download";
        interface = with lib; {
          options = {
            acme = {
              email = mkOption {
                type = types.str;
                description = "let's encrypt account name";
              };
              acceptTerms = mkEnableOption "Accept let's encrypt terms";
              server = mkOption {
                type = types.str;
                default = "https://acme-staging-v02.api.letsencrypt.org/directory";
                description = "ACME server to use";
              };
            };
          };
        };

        perInstance =
          {
            instanceName,
            settings,
            machines,
            roles,
            ...
          }:
          {
            nixosModule =
              { config, pkgs, ... }:
              {
                imports = [
                  self.nixosModules.impermanence
                ];

                impermanence.persist.directories = [ "/var/lib/acme" ];

                clan.core.vars.generators.acme-dns-challenge = {
                  prompts.cloudflare-token.description = "Cloudflare 'edit zone dns' token for dns-01 challenge";
                  files.cloudflare-token = { };
                  script = ''
                    cat $prompts/cloudflare-token > $out/cloudflare-token
                  '';
                };

                security.acme = {
                  acceptTerms = settings.acme.acceptTerms;
                  defaults = {
                    email = settings.acme.email;
                    server = settings.acme.server;
                    dnsProvider = "cloudflare";
                    dnsResolver = "sri.ns.cloudflare.com";
                    extraLegoRenewFlags = [
                      "--reuse-key"
                    ];
                    environmentFile = pkgs.writeText "cloudflare-settings" ''
                      CLOUDFLARE_POLLING_INTERVAL=60
                      CLOUDFLARE_PROPAGATION_TIMEOUT=3600
                    '';
                    credentialFiles.CF_DNS_API_TOKEN_FILE =
                      config.clan.core.vars.generators.acme-dns-challenge.files.cloudflare-token.path;
                  };

                  certs."self" = {
                    domain = "${config.networking.hostName}.${config.clan.core.settings.domain}";
                  };
                };
              };
          };
      };
      roles.client = {
        description = "will pull age-encrypted cert/key from server and decrypt it";
      };
    };
}
