{
  inputs,
  ...
}:
let
  homeboxTags = builtins.fromJSON (builtins.readFile ./homebox.json);
in
{
  flake.nixosModules.homebox =
    { config, pkgs, ... }:
    {
      key = "nixos-config.modules.nixos.homebox";

      imports = [
        inputs.arion.nixosModules.arion
      ];

      config = {
        # homebox >=0.26 fails fast unless auth.api_key_pepper is >=32 bytes.
        # Auto-generate it once and expose it as an env-file fragment the
        # container consumes via env_file. Rotating it invalidates all API keys.
        clan.core.vars.generators.homebox-auth = {
          files.homebox-env.secret = true;
          runtimeInputs = [ pkgs.openssl ];
          script = ''
            printf 'HBOX_AUTH_API_KEY_PEPPER=%s\n' "$(openssl rand -base64 48 | tr -d '\n')" > $out/homebox-env
          '';
        };

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
                env_file = [
                  config.clan.core.vars.generators.homebox-auth.files.homebox-env.path
                ];
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
