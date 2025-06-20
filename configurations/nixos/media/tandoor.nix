{ lib, pkgs, config, ... }:
{
  config = {
    sops.secrets."tandoor/secret-key" = { };
    sops.secrets."tandoor/postgres-password" = { };

    sops.templates.tandoor-env.content = ''
      SECRET_KEY=${config.sops.placeholder."tandoor/postgres-password"}

      # allowed hosts (see documentation), should be set to your hostname(s) but might be * (default) for some proxies/providers
      # ALLOWED_HOSTS=recipes.mydomain.com

      # add only a database password if you want to run with the default postgres, otherwise change settings accordingly
      DB_ENGINE=django.db.backends.postgresql
      POSTGRES_HOST=db_recipes
      POSTGRES_DB=djangodb
      POSTGRES_PORT=5432
      POSTGRES_USER=djangouser
      POSTGRES_PASSWORD=${config.sops.placeholder."tandoor/postgres-password"}
    '';

    services.caddy.virtualHosts."tandoor.binarin.info".extraConfig = ''
      file_server /media/* {
        root /var/lib/tandoor/mediafiles
      }

      reverse_proxy http://127.0.0.1:8081
      tls {
          dns cloudflare {file.{$CREDENTIALS_DIRECTORY}/cloudflare-api-token}
          resolvers 1.1.1.1
      }
    '';

    virtualisation.arion.projects.tandoor = {
      serviceName = "tandoor-docker-compose";
      settings.docker-compose.volumes.nginx_config = {};
      settings.services = {
        db_recipes.service = {
          restart = "unless-stopped";
          image = "postgres:16-alpine";
          volumes = [
            "/var/lib/tandoor/postgresql:/var/lib/postgresql/data"
          ];
          env_file = [
            config.sops.templates.tandoor-env.path
          ];
        };
        web_recipes.service = {
          restart = "unless-stopped";
          image = "vabene1111/recipes";
          ports = [
            "8081:8080"
          ];
          env_file = [
            config.sops.templates.tandoor-env.path
          ];
          volumes = [
            "/var/lib/tandoor/staticfiles:/opt/recipes/staticfiles"
            "/var/lib/tandoor/mediafiles:/opt/recipes/mediafiles"
          ];
          depends_on = [ "db_recipes" ];
        };
      };
    };
  };
}
