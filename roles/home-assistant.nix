{config, pkgs, ...}:

{
  imports = [
  ];
  environment.systemPackages = [
  ];
  services.home-assistant = {
    enable = true;
    config = {
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
        api_password = "!secret http_password";
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
        password = "!secret mqtt_password";
      };
    };
  };
}
