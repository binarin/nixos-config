{
  self,
  inputs,
  lib,
  config,
  ...
}:
let
  flakeConfig = config;

  # Repo-specific overlays, applied on top of the nixpkgs evaluation shared via
  # common (its modules/nixpkgs.nix owns the nixpkgs config, the Emacs and
  # zwanenzang overlays, and llm-agents on nixpkgs-unstable). Reusing that
  # evaluation instead of importing nixpkgs again keeps one instance per system.
  nixosOverlays = [
    inputs.deploy-rs.overlays.default

    inputs.emacs-overlay.overlays.default

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
    self.overlays.paseo

    self.overlays.foot-no-strip-file-url

    self.overlays.immich-stack
    self.overlays.imageio-pillow

    self.overlays.niri
  ];

in
{
  config = {
    flake.configured-pkgs = inputs.common.configured-pkgs;

    # Package sets: common evaluates nixpkgs, this repo only appends its
    # overlays and the `bleeding` aliases. Per system because flake outputs are
    # generated for x86_64-linux and aarch64-linux.
    flake.lib.pkgsFor =
      system:
      let
        common = inputs.common.configured-pkgs.${system};
        unstable = common.nixpkgs-unstable.appendOverlays nixosOverlays;
      in
      common.nixpkgs.appendOverlays (
        nixosOverlays
        ++ [
          (_final: _prev: {
            bleeding = unstable;
            trezor-agent = unstable.trezor-agent;
            bleeding-cuda = common.nixpkgs-unstable-cuda.appendOverlays nixosOverlays;
          })
        ]
      );

    flake.lib.nixpkgs = self.lib.pkgsFor "x86_64-linux";

    perSystem =
      { system, ... }:
      {
        _module.args.pkgs = self.lib.pkgsFor system;
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
          site: _: config.clan.core.vars.generators."${genName site}".files.token.path
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
            before = [ "nix-daemon.service" ];
            unitConfig.ConditionPathExists = lib.attrValues tokenFiles;
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              RuntimeDirectory = "nix-access-tokens";
              # World-executable so that any user's nix client can traverse into
              # the directory to read the `!include`d file below (the file
              # itself stays 0440 root:nix-access-tokens). If nix cannot
              # resolve the include, it silently discards the whole nix.conf,
              # which e.g. disables `nix-command` for non-root users.
              RuntimeDirectoryMode = "0755";
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
