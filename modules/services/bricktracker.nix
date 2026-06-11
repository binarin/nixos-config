{
  inputs,
  self,
  ...
}:
let
  bricktrackerTags = builtins.fromJSON (builtins.readFile ./bricktracker.json);
in
{
  flake.nixosModules.bricktracker =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.bricktracker";

      imports = [
        inputs.sops-nix.nixosModules.sops
        inputs.arion.nixosModules.arion
      ];

      config = {
        sops.secrets."bricktracker/rebrickable-api-key" = { };
        sops.templates."bricktracker-env".content = ''
          BK_REBRICKABLE_API_KEY="${config.sops.placeholder."bricktracker/rebrickable-api-key"}"
        '';

        virtualisation.arion.backend = "docker";
        virtualisation.arion.projects.bricktracker = {
          serviceName = "bricktracker-docker-compose";
          settings = {
            services.bricktracker = {
              service = {
                container_name = "BrickTracker";
                restart = "unless-stopped";
                image = "gitea.baerentsen.space/frederikbaerentsen/bricktracker:${bricktrackerTags.bricktracker}";
                ports = [ "3333:3333" ];
                volumes = [
                  "/persist/BrickTracker/data:/data/"
                  "/persist/BrickTracker/instructions:/app/static/instructions/"
                  "/persist/BrickTracker/minifigures:/app/static/minifigures/"
                  "/persist/BrickTracker/parts:/app/static/parts/"
                  "/persist/BrickTracker/sets:/app/static/sets/"
                ];
                environment = {
                  BK_DATABASE_PATH = "/data/app.db";
                  BK_MINIFIGURES_FOLDER = "minifigures";
                  BK_RETIRED_SETS_PATH = "/data/retired_sets.csv";
                  BK_THEMES_PATH = "/data/themes.csv";
                };
                env_file = [
                  config.sops.templates.bricktracker-env.path
                ];
              };
            };
          };
        };
      };
    };
}
