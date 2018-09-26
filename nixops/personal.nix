{
  network.description = "personal servers";
  kodi = {config, lib, pkgs, ...}: {
     imports = [
       ../configuration.nix-kodi
     ];
  };
  balthamel = {config, lib, pkgs, ...}: {
     imports = [
       ../configuration.nix-balthamel
     ];
  };
  naberius = {config, lib, pkgs, ...}: {
     imports = [
       ../configuration.nix-naberius
     ];
    deployment.keys = {
      gitlab-database-password = {
        text = builtins.extraBuiltins.pass "deploy/gitlab/database-password";
      };
      gitlab-db-key-base = {
        text = builtins.extraBuiltins.pass "deploy/gitlab/db_key_base";
      };
      gitlab-otp-key-base = {
        text = builtins.extraBuiltins.pass "deploy/gitlab/otp_key_base";
      };
      gitlab-secret-key-base = {
        text = builtins.extraBuiltins.pass "deploy/gitlab/secret_key_base";
      };
      gitlab-naberius-runner-token = {
        text = builtins.extraBuiltins.pass "deploy/gitlab/naberius_runner_token";
      };
      nix-store-secret = {
        text = builtins.extraBuiltins.pass "deploy/naberius-nix-store/secret";
        path = "/root/.nix-store-signing-key";
      };
    };
  };
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
