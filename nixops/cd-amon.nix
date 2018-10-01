{
  network.description = "Deployment for CD pipeline for amon";
  amon = {config, lib, pkgs, ...}: {
    imports = [
      ../configuration.nix-amon
    ];
    deployment.keys = builtins.listToAttrs (
      map (k: {
        name = k;
        value = {
          text = builtins.getEnv "DEPLOY_${k}";
          user = "hass";
          permissions = "0400";
        };
      }) [
        "hass_http_password"
        "hass_mqtt_password"
        "hass_kodi_password"
        "hass_netatmo_api_key"
        "hass_netatmo_secret_key"
        "hass_netatmo_username"
        "hass_netatmo_password"
      ]
    );
  };
}
