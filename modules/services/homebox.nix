{
  inputs,
  ...
}:
let
  homeboxTags = builtins.fromJSON (builtins.readFile ./homebox.json);
in
{
  flake.nixosModules.homebox =
    { lib, ... }:
    {
      key = "nixos-config.modules.nixos.homebox";

      imports = [
        inputs.arion.nixosModules.arion
      ];

      config = {
        virtualisation.arion.backend = "docker";
        virtualisation.arion.projects.homebox = {
          serviceName = "home-box-docker-compose";
          settings = {
            services.homebox = {
              service = {
                image = "ghcr.io/sysadminsmedia/homebox:${homeboxTags.homebox}";
                container_name = "homebox";
                restart = "unless-stopped";
                environment = {
                  HBOX_LOG_LEVEL = "info";
                  HBOX_LOG_FORMAT = "text";
                  HBOX_WEB_MAX_FILE_UPLOAD = "10";
                  HBOX_OPTIONS_ALLOW_REGISTRATION = "false";
                  HBOX_MODE = "production";
                  HBOX_STORAGE_DATA = "/data";
                  HBOX_DATABASE_DRIVER = "sqlite3";
                  HBOX_STORAGE_SQLITE_PATH = "/data/homebox.db?_pragma=busy_timeout=999&_pragma=journal_mode=WAL&_fk=1";
                };
                volumes = [
                  "/persist/homebox/data:/data/"
                ];
                ports = [
                  "7745:7745"
                ];
              };
            };
          };
        };
      };
    };
}
