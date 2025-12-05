{ inputs, ... }:
{
  flake.nixosModules.archivebox =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.archivebox";

      # Explicit imports for all dependencies this module needs
      imports = [
        inputs.sops-nix.nixosModules.sops
        inputs.arion.nixosModules.arion
      ];

      config = {
        # Create archivebox user with fixed UID/GID for Docker volume permissions
        users.groups.archivebox = {
          gid = 2001;
        };

        users.users.archivebox = {
          isSystemUser = true;
          group = "archivebox";
          uid = 2001;
        };

        # Sops secrets configuration
        sops.secrets."archivebox-admin-username" = { };
        sops.secrets."archivebox-admin-password" = { };
        sops.secrets."archivebox-search-password" = { };

        # Sops template for environment variables
        sops.templates."archivebox-env".content = ''
          ADMIN_USERNAME=${config.sops.placeholder."archivebox-admin-username"}
          ADMIN_PASSWORD=${config.sops.placeholder."archivebox-admin-password"}
          SEARCH_BACKEND_PASSWORD=${config.sops.placeholder."archivebox-search-password"}
          ALLOWED_HOSTS=*
          PUBLIC_INDEX=True
          PUBLIC_SNAPSHOTS=True
          PUBLIC_ADD_VIEW=False
          SEARCH_BACKEND_ENGINE=sonic
          SEARCH_BACKEND_HOST_NAME=sonic
          PUID=2001
          PGID=2001
        '';

        # Systemd tmpfiles rules for directory creation
        # Mount structure: Only 2 host mounts
        # - /mnt/archivebox/data: archived data
        # - /mnt/archivebox/var: everything else (with subfolders per container)
        systemd.tmpfiles.rules = [
          "d /mnt/archivebox 0755 archivebox archivebox -"
          "d /mnt/archivebox/var 0755 archivebox archivebox -"
          "d /mnt/archivebox/var/archivebox 0755 archivebox archivebox -"
          "d /mnt/archivebox/var/sonic 0755 archivebox archivebox -"
          "d /mnt/archivebox/data 0755 archivebox archivebox -"
        ];

        # Arion docker-compose configuration
        virtualisation.arion.backend = "docker";
        virtualisation.arion.projects.archivebox = {
          serviceName = "archivebox-docker-compose";
          settings = {
            services = {
              # Main ArchiveBox service
              archivebox = {
                service = {
                  image = "archivebox/archivebox:latest";
                  container_name = "archivebox";
                  restart = "unless-stopped";
                  ports = [ "8000:8000" ];
                  volumes = [
                    "/mnt/archivebox/var/archivebox:/data"
                    "/mnt/archivebox/data:/data/archive"
                  ];
                  env_file = [
                    config.sops.templates.archivebox-env.path
                  ];
                  depends_on = [ "sonic" ];
                };
              };

              # Scheduler service for periodic archiving
              archivebox_scheduler = {
                service = {
                  image = "archivebox/archivebox:latest";
                  container_name = "archivebox_scheduler";
                  restart = "unless-stopped";
                  command = [
                    "schedule"
                    "--foreground"
                    "--update"
                    "--every=day"
                  ];
                  volumes = [
                    "/mnt/archivebox/var/archivebox:/data"
                    "/mnt/archivebox/data:/data/archive"
                  ];
                  environment = {
                    TIMEOUT = "120";
                  };
                  env_file = [
                    config.sops.templates.archivebox-env.path
                  ];
                  depends_on = [ "sonic" ];
                };
              };

              # Sonic search backend
              sonic = {
                service = {
                  image = "archivebox/sonic:latest";
                  container_name = "sonic";
                  restart = "unless-stopped";
                  expose = [ "1491" ];
                  volumes = [
                    "/mnt/archivebox/var/sonic:/var/lib/sonic/store"
                  ];
                  environment = {
                    SEARCH_BACKEND_PASSWORD = config.sops.placeholder."archivebox-search-password";
                  };
                };
              };
            };
          };
        };
      };
    };
}
