{ ... }:
{
  services.caddy.expose-local-http.virtualHosts."homebox.binarin.info" = "http://localhost:7745";
  virtualisation.arion.projects.homebox = {
    serviceName = "home-box-docker-compose";
    settings = {
      services = {
        homebox = {
          service = {
            image = "ghcr.io/sysadminsmedia/homebox:0.21";
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
}
