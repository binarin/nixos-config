{config, pkgs, lib,  ...}:

with lib;

let
  lnetatmo = ps: ps.buildPythonPackage {
    pname = "lnetatmo";
    version = "0.9.2.1";
    src = pkgs.fetchurl {
      url = https://github.com/jabesq/netatmo-api-python/archive/v0.9.2.1.zip;
      sha256 = "10vf3szl9x09d3fbxfpsjac5i295nmhgc0g044rp9gp653i3b581";
    };
    patches = [
      ./hass-lnetatmo.patch
    ];
    meta = with pkgs.stdenv.lib; {
      homepage = "https://github.com/philippelt/netatmo-api-python";
      license = "GPL V3";
      description = "Simple API to access Netatmo weather station data from any python script.";
    };
  };
  hassPackage = pkgs.bleeding.home-assistant.override {
    extraPackages = ps: with ps; [
      xmltodict paho-mqtt netdisco (lnetatmo ps) keyring keyrings-alt jsonrpc-async jsonrpc-websocket
    ];
  };
in {
  services.home-assistant = {
    enable = true;
    package = hassPackage;
  };
  users.extraUsers.hass.extraGroups = [ "keys" ];
  systemd.services.home-assistant.preStart = ''
    KEYS=(
      hass_http_password
      hass_mqtt_password
      hass_kodi_password
      hass_netatmo_api_key
      hass_netatmo_secret_key
      hass_netatmo_username
      hass_netatmo_password
    )

    for key in "''${KEYS[@]}"; do
      if [[ -f /run/keys/$key ]]; then
          cat /run/keys/$key | ${hassPackage}/bin/hass --script keyring set $key
      fi
    done

    ln -sf ${./home-assistant/configuration.yaml} /var/lib/hass/configuration.yaml
    ln -sf ${./home-assistant/customize.yaml} /var/lib/hass/customize.yaml
    ln -sf ${./home-assistant/groups.yaml} /var/lib/hass/groups.yaml
    ln -sf ${./home-assistant/automations.yaml} /var/lib/hass/automations.yaml
    ln -sf ${./home-assistant/scripts.yaml} /var/lib/hass/scripts.yaml
  '';

  services.nginx.upstreams.hass-backend.servers = {
    "http://127.0.0.1:8123" = {};
  };

  services.nginx.virtualHosts."amon.binarin.ru".locations."/hass" = {
    proxyPass = "http://hass-backend";
    extraConfig = ''
      proxy_http_version 1.1;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection $connection_upgrade;
    '';
  };
}
