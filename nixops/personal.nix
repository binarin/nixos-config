{
  network.description = "personal servers";
  kodi = {config, lib, pkgs, ...}: {
     imports = [
       ../configuration.nix-kodi
     ];
  };
  amon = {config, lib, pkgs, ...}: {
     imports = [
       ../configuration.nix-amon
     ];
     deployments.keys.hass-http-password = {
       text = builtins.getEnv "DEPLOY_HASS_HTTP_PASSWORD";
       user = "hass";
       permissions = "0400";
     };
     deployments.keys.hass-mqtt-password = {
       text = builtins.getEnv "DEPLOY_HASS_MQTT_PASSWORD";
       user = "hass";
       permissions = "0400";
     };
     deployments.keys.kodi-api-password = {
       text = builtins.getEnv "DEPLOY_HASS_KODI_PASSWORD";
       user = "hass";
       permissions = "0400";
     };
  };
}
