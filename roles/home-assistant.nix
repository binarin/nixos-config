{config, pkgs, lib,  ...}:

with lib;

let

  pyatmo = ps: ps.buildPythonPackage {
    pname = "pyatmo";
    version = "1.2.1";
    src = pkgs.fetchurl {
      url = https://files.pythonhosted.org/packages/77/66/e1c9e95ae60ed31cdbec6c9d889eb37da175d736aedda449ef4743471528/pyatmo-1.2.1.tar.gz;
      sha256 = "0wwprngw88w7bjccik34bii5c5m5xpw4bg9qyxa5xkx194rlm78k";
    };
    meta = with pkgs.stdenv.lib; {
      homepage = "https://pypi.org/project/pyatmo/";
      license = "MIT";
      description = "Simple API to access Netatmo weather station data from any python script.";
    };
  };
  hassPackage = pkgs.bleeding.home-assistant.override {
    extraPackages = ps: with ps; [
      xmltodict paho-mqtt netdisco (pyatmo ps) keyring keyrings-alt jsonrpc-async jsonrpc-websocket aiohue
    ];
  };
in {
  services.home-assistant = {
    enable = true;
    package = hassPackage;
  };
  users.extraUsers.hass.extraGroups = [ "keys" ];
  systemd.services.home-assistant.after = [ "rabbitmq.service" ];
  systemd.services.home-assistant.requires = [ "rabbitmq.service" ];

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

  services.nginx.upstreams."hass.binarin.ru".servers = {
    "192.168.2.14:8123" = {};
  };

  services.nginx.virtualHosts."hass.binarin.ru" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "https://hass.binarin.ru";
      extraConfig = ''
        proxy_set_header Connection $connection_upgrade;
        proxy_set_header Host $host;
        proxy_redirect http:// https://;
        proxy_http_version 1.1;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;
      '';
    };
  };

  services.nginx.virtualHosts."hass-dev.binarin.ru" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://10.48.168.180:8123";
      extraConfig = ''
        proxy_set_header Connection $connection_upgrade;
        proxy_set_header Host $host;
        proxy_redirect http:// https://;
        proxy_http_version 1.1;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;
      '';
    };
  };

  services.nginx.virtualHosts."hass-dash.binarin.ru" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://10.48.168.180:5050";
      extraConfig = ''
        proxy_set_header Connection $connection_upgrade;
        proxy_set_header Host $host;
        proxy_redirect http:// https://;
        proxy_http_version 1.1;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;
      '';
    };
  };

}
