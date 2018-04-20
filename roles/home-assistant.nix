{config, pkgs, lib,  ...}:

with lib;

let
  hassConfig = {
    homeassistant = {
      name = "Meer en Vaart";
      latitude = "52.379189";
      longitude = "4.899431";
      elevation = "0";
      unit_system = "metric";
      time_zone = "Europe/Amsterdam";
    };
    recorder = {
      db_url = "sqlite:////var/lib/hass/hass.sqlite3";
    };
    frontend = {
    };
    config = {
    };
    http = {
      api_password = "$(cat /run/keys/hass-http-password)";
    };
    updater = {
    };
    discovery = {
    };
    conversation = {
    };
    history = {
    };
    logbook = {
    };
    map = {
    };
    sun = {
    };
    sensor = {
      platform = "yr";
    };
    tts = {
      platform = "google";
    };
    cloud = {
    };
    mqtt = {
      broker = "127.0.0.1";
      username = "led-strip-1";
      password = "$(cat /run/keys/hass-mqtt-password)";
    };
    logger = {
      default = "debug";
    };
  };
  keyDeps = [ "hass-http-password-key.service" "hass-mqtt-password-key.service" ];
  availableComponents = pkgs.home-assistant.availableComponents;

  useComponentPlatform = component:
    let
      path = splitString "." component;
      parentConfig = attrByPath (init path) null hassConfig;
      platform = last path;
    in isList parentConfig && any
      (item: item.platform or null == platform)
      parentConfig;

  # Returns whether component is used in config
  useComponent = component:
    hasAttrByPath (splitString "." component) hassConfig
    || useComponentPlatform component;

  # List of components used in config
  extraComponents = filter useComponent availableComponents;
in {

  systemd.services.hass-config-renderer = {
    after = keyDeps;
    wants = keyDeps;
    wantedBy = [ "home-assistant.service" ];
    serviceConfig = {
      Type = "oneshot";
    };
    script = ''
      #!${pkgs.bash}/bin/bash
      CONFIG=${config.services.home-assistant.configDir}/configuration.yaml
      rm -f $CONFIG
      cat <<EOF > $CONFIG
      ${builtins.toJSON hassConfig}
      EOF
      chown hass:hass $CONFIG
      chmod 0400 $CONFIG
    '';
  };

  services.home-assistant = {
    enable = true;
    package = pkgs.home-assistant.override {
      inherit extraComponents;
      extraPackages = ps: with ps; [ xmltodict ];
    };
  };
}
