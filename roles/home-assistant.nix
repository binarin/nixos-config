{config, pkgs, ...}:

let
  keyringsAlt = ps: ps.buildPythonPackage {
      pname = "keyrings.alt";
      version = "2.3";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/90/2d/4425b56231a1bbd8dc5bb6549d4558920abfe1dea97eaf9dc92845d7438d/keyrings.alt-2.3.tar.gz"; sha256 = "5cb9b6cdb5ce5e8216533e342d3e1b418ddd210466834061966d7dc1a4736f2d"; };
      doCheck = false;
      propagatedBuildInputs = with ps; [
        keyring six setuptools_scm
      ];
  };
in {
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
      logger = {
        default = "debug";
      };
    };
    package = pkgs.home-assistant.override {
      extraPackages = ps: with ps; [ xmltodict (keyringsAlt ps)];
    };
  };
}
