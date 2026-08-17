{
  self,
  inputs,
  lib,
  config,
  ...
}:
let
  flakeConfig = config;
  nixpkgsConfig = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
    permittedInsecurePackages = [
      "qtwebengine-5.15.19"
      "python3.12-ecdsa-0.19.1"
      "python3.13-ecdsa-0.19.1"
      "olm-3.2.16"
    ];
  };

  defaultOverlays = [
    inputs.deploy-rs.overlays.default

    inputs.emacs-overlay.overlays.default
    inputs.nix-ai-tools.overlays.shared-nixpkgs

    # gcx 0.4.3 — newer than nixpkgs
    (final: prev: {
      gcx = prev.gcx.overrideAttrs (finalAttrs: {
        version = "0.4.3";
        src = prev.fetchFromGitHub {
          owner = "grafana";
          repo = "gcx";
          tag = "v${finalAttrs.version}";
          hash = "sha256-gN3l45wFpZSUnhprei/Ca1/4ptmOFtpNmNUpy6sn0aU";
        };
      });
    })

    self.overlays.jerk-gpa
    self.overlays.ksso
    self.overlays.klaude

    self.overlays.sicstus-manual
    self.overlays.shitty-shit-launcher
    self.overlays.slack
    self.overlays.my-emacs
    self.overlays.my-google-chrome
    self.overlays.waybar-org-clock
    self.overlays.wprintidle-c
    self.overlays.brownnoise
    self.overlays.git-crypt-patched

    self.overlays.foot-no-strip-file-url

    self.overlays.immich-stack

    self.overlays.niri
  ];

  importNixpkgs =
    {
      input,
      system,
      extraOverlays ? [ ],
      extraConfig ? { },
    }:
    import input {
      inherit system;
      config = nixpkgsConfig // extraConfig;
      overlays = extraOverlays ++ defaultOverlays;
    };

in
{
  config = {
    flake-file.inputs = {
      flake-compat = {
        url = "github:NixOS/flake-compat";
      };
    };

    flake.lib.importNixpkgs = importNixpkgs;

    flake.configured-pkgs = lib.genAttrs [ "x86_64-linux" "aarch64-linux" ] (system: rec {
      nixpkgs-unstable = importNixpkgs {
        inherit system;
        input = inputs.nixpkgs-unstable;
      };

      nixpkgs-unstable-cuda = importNixpkgs {
        inherit system;
        input = inputs.nixpkgs-unstable;
        extraConfig.cudaSupport = true;
      };

      nixpkgs = importNixpkgs {
        inherit system;
        input = inputs.nixpkgs;
        extraOverlays = [
          (final: _prev: {
            bleeding = nixpkgs-unstable;
            trezor-agent = nixpkgs-unstable.trezor-agent;
            bleeding-cuda = nixpkgs-unstable-cuda;
          })
        ];
      };

    });

    perSystem =
      { system, ... }:
      {
        _module.args.pkgs = self.configured-pkgs."${system}".nixpkgs;
      };

    # The `nix` module is backend-neutral. The access-token *secret backend* is
    # selected at the imports level from `specialArgs` (never with an in-config
    # `mkIf`), mirroring how modules/baseline/default.nix conditionally imports
    # clan-baseline. Each backend renders its own extra-access-tokens file and
    # wires the `!include` into nix.extraOptions.
    flake.nixosModules.nix =
      {
        lib,
        config,
        modulesPath,
        pkgs,
        specialArgs,
        ...
      }:
      {
        key = "nixos-config.modules.nixos.nix";

        options.nixos-config.nix.accessTokens = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = { };
          description = ''
            Attrset mapping hostnames to nix access token secrets. On non-clan
            machines the value is the sops secret name; on clan machines the value
            is ignored (a shared clan-vars generator is derived from the site).
            Example: { "github.com" = "extra-access-tokens/github.com"; }
          '';
        };

        imports = [
          (
            if specialArgs ? clan-core then
              self.nixosModules.nix-access-tokens-clan
            else
              self.nixosModules.nix-access-tokens-sops
          )
        ];

        config = {
          nix = {
            settings = {
              sandbox = true;
              substituters = [ "https://cache.nixos.org" ];
            };
            extraOptions = ''
              experimental-features = nix-command flakes
            '';
          };

          users.groups.nix-access-tokens = { };
          nix.settings.trusted-users = [ "root" ];
        };
      };

    flake.nixosModules.nix-access-tokens-sops =
      { lib, config, ... }:
      let
        cfg = config.nixos-config.nix.accessTokens;
        hasTokens = cfg != { };
        tokenLine = lib.concatStringsSep " " (
          lib.mapAttrsToList (site: name: "${site}=${config.sops.placeholder.${name}}") cfg
        );
      in
      {
        key = "nixos-config.modules.nixos.nix-access-tokens-sops";
        config = lib.mkIf hasTokens {
          sops.secrets = lib.mapAttrs' (_site: secretName: {
            name = secretName;
            value = { };
          }) cfg;

          sops.templates."nix-access-tokens" = {
            content = "extra-access-tokens = ${tokenLine}\n";
            group = "nix-access-tokens";
            mode = "0440";
          };

          nix.extraOptions = ''
            !include ${config.sops.templates."nix-access-tokens".path}
          '';
        };
      };

    flake.nixosModules.nix-access-tokens-clan =
      { lib, config, ... }:
      let
        cfg = config.nixos-config.nix.accessTokens;
        genName = site: "nix-access-token-${lib.replaceStrings [ "." ] [ "-" ] site}";
        tokenFiles = lib.mapAttrs (
          site: _:
          config.clan.core.vars.generators."${genName site}".files.token.path
        ) cfg;
      in
      {
        key = "nixos-config.modules.nixos.nix-access-tokens-clan";
        config = lib.mkIf (cfg != { }) {
          clan.core.vars.generators = lib.mapAttrs' (
            site: _:
            lib.nameValuePair (genName site) {
              share = true;
              prompts.token.description = "nix access token for ${site} (e.g. GitHub PAT)";
              files.token = {
                secret = true;
                deploy = true;
                restartUnits = [ "nix-access-tokens.service" ];
              };
              script = ''
                tr -d '\n' < "$prompts/token" > "$out/token"
              '';
            }
          ) cfg;

          systemd.services.nix-access-tokens = {
            description = "Render nix extra-access-tokens file";
            wantedBy = [ "multi-user.target" ];
            before = [
              "nix-daemon.service"
              "nix-daemon.socket"
            ];
            unitConfig.ConditionPathExists = lib.attrValues tokenFiles;
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              RuntimeDirectory = "nix-access-tokens";
              RuntimeDirectoryMode = "0750";
              UMask = "0077";
            };
            script = ''
              line="extra-access-tokens ="
              ${lib.concatStringsSep "\n" (
                lib.mapAttrsToList (site: path: ''line+=" ${site}=$(<${path})"'') tokenFiles
              )}
              printf '%s\n' "$line" > /run/nix-access-tokens/extra-access-tokens
              chgrp nix-access-tokens /run/nix-access-tokens/extra-access-tokens
              chmod 0440 /run/nix-access-tokens/extra-access-tokens
            '';
          };

          nix.extraOptions = ''
            !include /run/nix-access-tokens/extra-access-tokens
          '';
        };
      };
  };
}
