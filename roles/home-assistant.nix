{config, pkgs, lib,  ...}:

with lib;

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
  hassPackage = pkgs.home-assistant.override {
    extraPackages = ps: with ps; [
      xmltodict paho-mqtt netdisco (lnetatmo ps) (keyringsAlt ps) jsonrpc-async jsonrpc-websocket
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
}
