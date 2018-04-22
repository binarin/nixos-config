# export NIX_PATH=nixpkgs=/etc/nixos/nixpkgs:.
export NIXOPS_STATE=~/org/deployments.nixops

KEYS=(
  hass_http_password
  hass_mqtt_password
  hass_kodi_password
  hass_netatmo_api_key
  hass_netatmo_secret_key
  hass_netatmo_username
  hass_netatmo_password
)

for key in "${KEYS[@]}"; do
    export DEPLOY_$key=$(gopass show deploy/$key)
done
