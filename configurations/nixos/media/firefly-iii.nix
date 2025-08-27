{ lib, config, pkgs, ... }:
{
  config = {
    sops.secrets."firefly-iii/app_key" = {
      owner = config.services.firefly-iii.user;
      group = config.services.firefly-iii.group;
      mode = "0400";
    };

    sops.secrets."smtp2go/username" = {
      sopsFile = "${config.lib.self.file' "secrets/webservers.yaml"}";
      owner = config.services.firefly-iii.user;
      group = config.services.firefly-iii.group;
      mode = "0400";
    };

    sops.secrets."smtp2go/password" = {
      sopsFile = "${config.lib.self.file' "secrets/webservers.yaml"}";
      owner = config.services.firefly-iii.user;
      group = config.services.firefly-iii.group;
      mode = "0400";
    };

    services.firefly-iii = {
      enable = true;
      virtualHost = "ff.binarin.info";
      enableNginx = true;
      settings = {
        DB_CONNECTION = "sqlite";

        APP_KEY_FILE = config.sops.secrets."firefly-iii/app_key".path;
        APP_URL = "https://ff.binarin.info";
        TRUSTED_PROXIES = "**";

        MAIL_MAILER = "smtp";
        MAIL_HOST = "mail.smtp2go.com";
        MAIL_PORT = 465;
        MAIL_FROM = "firefly-iii@binarin.info";
        MAIL_ENCRYPTION = "ssl";
        MAIL_USERNAME_FILE = config.sops.secrets."smtp2go/username".path;
        MAIL_PASSWORD_FILE = config.sops.secrets."smtp2go/password".path;
      };
    };

    services.nginx.virtualHosts."ff.binarin.info".listen = [
      {
        addr = "127.0.0.1";
        port = 64085;
      }
    ];

    services.caddy.virtualHosts."ff.binarin.info".extraConfig = ''
      reverse_proxy http://127.0.0.1:64085

      import letsencrypt
    '';
  };
}
