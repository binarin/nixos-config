# -*- nix -*-
{
  flake,
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (flake) inputs;
  inherit (inputs) self;
  forgejoPackage = pkgs.bleeding.forgejo-lts;
in {
  imports = [
    self.nixosModules.server

    {
      # use forgejo module from nixpkgs-master (and the compatible version of forgejo itself)
      disabledModules = ["services/misc/forgejo.nix"];
      imports = ["${inputs.nixpkgs-unstable}/nixos/modules/services/misc/forgejo.nix"];
      nixpkgs.overlays = [
        (final: prev: {
          forgejo = prev.bleeding.forgejo-lts;
          forgejo-lts = prev.bleeding.forgejo-lts;
        })
      ];
    }
  ];

  hostConfig.feature.bleeding = lib.mkForce true;

  sops.secrets.tailscale-auth = {};
  services.tailscale = {
    enable = true;
    authKeyFile = "${config.sops.secrets.tailscale-auth.path}";
    extraUpFlags = [
      "--hostname"
      "${config.networking.hostName}"
    ];
  };

  environment.systemPackages = with pkgs; [emacs-nox];

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  sops.secrets.smtp2go-username = {};
  sops.secrets.smtp2go-password = {};

  users.users.git = {
    home = config.services.forgejo.stateDir;
    group = "forgejo";
    useDefaultShell = true;
    isSystemUser = true;
  };

  services.forgejo = {
    enable = true;
    user = "git";

    settings = {
      service = {
        DISABLE_REGISTRATION = true;
      };
      repository = {
        DEFAULT_BRANCH = "master";
        DEFAULT_PRIVATE = "private";
      };
      server = {
        HTTP_ADDR = "127.0.0.1";
        DOMAIN = "forgejo.lynx-lizard.ts.net";
        ROOT_URL = "https://forgejo.lynx-lizard.ts.net/";
      };
      mailer = {
        ENABLED = true;
        PROTOCOL = "smtps";
        SMTP_ADDR = "mail.smtp2go.com";
        SMTP_PORT = 465;
        FROM = "host-forgejo@binarin.info";
      };
      time = {
        DEFAULT_UI_LOCATION = "Europe/Amsterdam";
      };
    };
    secrets = {
      mailer = {
        USER = "${config.sops.secrets.smtp2go-username.path}";
        PASSWD = "${config.sops.secrets.smtp2go-password.path}";
      };
    };
  };

  system.stateVersion = "24.05";
}
