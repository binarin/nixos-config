# -*- nix -*-
{
  flake,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (flake) inputs;
  inherit (inputs) self;
in {
  networking.hostName = "monitor";

  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "yes";

  sops.secrets.tailscale-auth = {};

  services.tailscale = {
    enable = true;
    authKeyFile = "/run/secrets/tailscale-auth";
    extraUpFlags = [
      "--hostname"
      "${config.networking.hostName}"
    ];
  };

  environment.systemPackages = with pkgs; [emacs-nox];

  users.users."root".openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMCVAKqmUdCkJ1gbi2ZA6vLnmf880U/9v5bfxhChapWB binarin@nixos"
  ];

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  services.victoriametrics = {
    enable = true;
    listenAddress = "0.0.0.0:8428";
  };
  networking.firewall.allowedTCPPorts = [
    8428
    3000
  ];

  sops.secrets."grafana/admin-username" = {
    owner = config.users.users.grafana.name;
  };
  sops.secrets."grafana/admin-password" = {
    owner = config.users.users.grafana.name;
  };
  sops.secrets."grafana/secret-key" = {
    owner = config.users.users.grafana.name;
  };

  services.grafana = {
    enable = true;
    declarativePlugins = [pkgs.grafana-victoriametrics-datasource];
    settings = {
      plugins = {
        allow_loading_unsigned_plugins = "victoriametrics-datasource";
      };
      security = {
        admin_user = "$__file{${config.sops.secrets."grafana/admin-username".path}}";
        admin_password = "$__file{${config.sops.secrets."grafana/admin-password".path}}";
        secret_key = "$__file{${config.sops.secrets."grafana/secret-key".path}}";
      };
    };
    provision = {
      enable = true;
      datasources.settings = {
        apiVersion = 1;
        datasources = [
          {
            name = "VictoriaMetrics";
            type = "victoriametrics-datasource";
            access = "proxy";
            url = "http://127.0.0.1:8428";
            isDefault = true;
          }
        ];
      };
    };
  };

  system.stateVersion = "24.05";
}
