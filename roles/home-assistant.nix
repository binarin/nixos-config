{config, pkgs, ...}:

{
  imports = [
  ];
  environment.systemPackages = [
  ];
  services.home-assistant = {
    enable = false;
    config = {
      homeassistant = {
        name = "Meer en Vaart";
        latitude = "52.379189";
        longitude = "4.899431";
        elevation = "0";
        unit_system = "metric";
        time_zone = "Europe/Amsterdam";
      };
    };
  };
}
