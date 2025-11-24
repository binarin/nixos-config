{ ... }:
{
  flake.nixosModules.linkwarden =
    { config, ... }:
    {
      key = "nixos-config.nixos.linkwarden";
      config = {
        sops.secrets."linkwarden/nextauth-secret" = { };
        sops.secrets."linkwarden/postgres-password" = { };

        services.caddy.virtualHosts."linkwarden.binarin.info".extraConfig = ''
          reverse_proxy http://127.0.0.1:3000
        '';

        sops.templates."linkwarden-env".content = ''
          NEXTAUTH_URL=http://localhost:3000/api/v1/auth
          NEXTAUTH_SECRET=${config.sops.placeholder."linkwarden/nextauth-secret"}
          POSTGRES_PASSWORD=${config.sops.placeholder."linkwarden/postgres-password"}
          NEXT_PUBLIC_DISABLE_REGISTRATION=true
          DISABLE_NEW_SSO_USERS=true
        '';

        sops.templates."linkwarden-database-url-env".content = ''
          DATABASE_URL="postgresql://postgres:${
            config.sops.placeholder."linkwarden/postgres-password"
          }@postgres:5432/postgres"
        '';

        virtualisation.arion.projects.linkwarden = {
          serviceName = "linkwarden-docker-compose";
          settings = {
            services = {
              postgres.service = {
                image = "postgres:16-alpine";
                env_file = [
                  config.sops.templates."linkwarden-env".path
                ];
                restart = "unless-stopped";
                volumes = [
                  "/var/lib/linkwarden/postgres-data:/var/lib/postgresql/data"
                ];
              };
              linkwarden.service = {
                env_file = [
                  config.sops.templates."linkwarden-env".path
                  config.sops.templates."linkwarden-database-url-env".path
                ];
                restart = "unless-stopped";
                image = "ghcr.io/linkwarden/linkwarden:latest";
                ports = [ "3000:3000" ];
                volumes = [
                  "/var/lib/linkwarden/linkwarden-data:/data/data"
                ];
                depends_on = [
                  "postgres"
                  "meilisearch"
                ];
              };
              meilisearch.service = {
                image = "getmeili/meilisearch:v1.12.8";
                restart = "unless-stopped";
                env_file = [
                  config.sops.templates."linkwarden-env".path
                ];
                volumes = [
                  "/var/lib/linkwarden/meilisearch-data:/meili_data"
                ];
              };
            };
          };
        };
      };
    };
}
