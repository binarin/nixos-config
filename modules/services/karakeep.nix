{
  inputs,
  self,
  config,
  ...
}:
let
  flakeConfig = config;
in
{
  flake.nixosModules.karakeep =
    { config, lib, ... }:
    let
      inherit (lib) mkDefault;
    in
    {
      key = "nixos-config.modules.nixos.karakeep";

      # Explicit imports for all dependencies this module needs
      imports = [
        inputs.sops-nix.nixosModules.sops
        inputs.arion.nixosModules.arion
        self.nixosModules.tailscale
      ];

      config = {
        # Create karakeep user with fixed UID/GID from inventory for Docker volume permissions
        users.groups.karakeep = {
          gid = flakeConfig.inventory.usersGroups.systemUsers.karakeep.gid;
        };

        users.users.karakeep = {
          isSystemUser = true;
          group = "karakeep";
          uid = flakeConfig.inventory.usersGroups.systemUsers.karakeep.uid;
        };

        # Sops secrets configuration
        sops.secrets."karakeep/nextauth-secret" = { };
        sops.secrets."karakeep/meilisearch-master-key" = { };
        sops.secrets."karakeep/openai-api-key" = { };

        # Sops template for environment variables
        sops.templates."karakeep-env" = {
          content = ''
            NEXTAUTH_URL=https://karakeep.lynx-lizard.ts.net
            NEXTAUTH_SECRET=${config.sops.placeholder."karakeep/nextauth-secret"}
            MEILI_ADDR=http://meilisearch:7700
            MEILI_MASTER_KEY=${config.sops.placeholder."karakeep/meilisearch-master-key"}
            BROWSER_WEB_URL=http://chrome:9222
            DATA_DIR=/data
            DISABLE_SIGNUPS=true
            MAX_ASSET_SIZE_MB=100
            CRAWLER_DOWNLOAD_BANNER_IMAGE=true
            CRAWLER_STORE_SCREENSHOT=true
            OPENAI_API_KEY=${config.sops.placeholder."karakeep/openai-api-key"}
            PUID=${toString flakeConfig.inventory.usersGroups.systemUsers.karakeep.uid}
            PGID=${toString flakeConfig.inventory.usersGroups.systemUsers.karakeep.gid}
          '';
          restartUnits = [ "karakeep-docker-compose.service" ];
        };

        sops.templates."karakeep-meili-env" = {
          content = ''
            MEILI_MASTER_KEY=${config.sops.placeholder."karakeep/meilisearch-master-key"}
            MEILI_NO_ANALYTICS=true
          '';
          restartUnits = [ "karakeep-docker-compose.service" ];
        };

        # Systemd tmpfiles rules for directory creation
        # Mount structure:
        # - /mnt/karakeep/data: application data
        # - /mnt/karakeep/var/meilisearch: search index data
        systemd.tmpfiles.rules = [
          "d /mnt/karakeep 0755 karakeep karakeep -"
          "d /mnt/karakeep/data 0755 karakeep karakeep -"
          "d /mnt/karakeep/var 0755 karakeep karakeep -"
          "d /mnt/karakeep/var/meilisearch 0755 karakeep karakeep -"
        ];

        # Arion docker-compose configuration
        virtualisation.arion.backend = "docker";
        virtualisation.arion.projects.karakeep = {
          serviceName = "karakeep-docker-compose";
          settings = {
            services = {
              # Main Karakeep service
              karakeep = {
                service = {
                  image = "ghcr.io/karakeep-app/karakeep:0.30.0";
                  container_name = "karakeep";
                  restart = "unless-stopped";
                  ports = [ "3000:3000" ];
                  volumes = [
                    "/mnt/karakeep/data:/data"
                  ];
                  env_file = [
                    config.sops.templates.karakeep-env.path
                  ];
                  depends_on = [
                    "meilisearch"
                    "chrome"
                  ];
                };
              };

              # Meilisearch search backend
              meilisearch = {
                service = {
                  image = "getmeili/meilisearch:v1.13.3";
                  container_name = "karakeep-meilisearch";
                  restart = "unless-stopped";
                  expose = [ "7700" ];
                  volumes = [
                    "/mnt/karakeep/var/meilisearch:/meili_data"
                  ];
                  env_file = [
                    config.sops.templates.karakeep-meili-env.path
                  ];
                };
              };

              # Chrome headless browser for page capture
              chrome = {
                service = {
                  image = "gcr.io/zenika-hub/alpine-chrome:124";
                  container_name = "karakeep-chrome";
                  restart = "unless-stopped";
                  expose = [ "9222" ];
                  command = [
                    "--no-sandbox"
                    "--disable-gpu"
                    "--disable-dev-shm-usage"
                    "--remote-debugging-address=0.0.0.0"
                    "--remote-debugging-port=9222"
                  ];
                };
              };
            };
          };
        };

        # Expose Karakeep as a tailscale service
        services.tailscale.serve = {
          enable = mkDefault true;
          services.karakeep = {
            serviceName = "karakeep";
            protocol = "https";
            target = "localhost:3000";
          };
        };
      };
    };
}
