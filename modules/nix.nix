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

    # hyprland 0.55
    inputs.hyprland.overlays.hyprland-packages
    inputs.hyprland.overlays.hyprland-extras
    inputs.hyprland-contrib.overlays.default

    # inputs.determinate-nix.overlays.default
    (final: prev: {
      nix = inputs.determinate-nix.packages."${prev.stdenv.hostPlatform.system}".default;
    })

    self.overlays.jerk-gpa
    self.overlays.ksso
    self.overlays.klaude

    self.overlays.sicstus-manual
    self.overlays.slack
    self.overlays.my-emacs
    self.overlays.my-google-chrome
    self.overlays.waybar-org-clock
    self.overlays.brownnoise
    self.overlays.git-crypt-patched

    self.overlays.foot-no-strip-file-url

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

      determinate-nix = {
        url = "https://flakehub.com/f/DeterminateSystems/nix-src/*";
        inputs = {
          # XXX nix-functional-tests are broken
          # nixpkgs.follows = "nixpkgs";

          flake-parts.follows = "flake-parts";

          # stop downloading those 2 old inputs
          nixpkgs-23-11.follows = "nixpkgs";
          nixpkgs-regression.follows = "nixpkgs";

          git-hooks-nix.inputs.flake-compat.follows = "flake-compat";
        };
      };

      determinate = {
        url = "https://flakehub.com/f/DeterminateSystems/determinate/*";
        inputs = {
          nixpkgs.follows = "nixpkgs";
          nix.follows = "determinate-nix";
        };
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
    # clan-baseline. Each backend companion sets `_accessTokenSecretNames`
    # (site -> sops placeholder key) and declares its own secret store; the shared
    # module renders one `extra-access-tokens` template from those names.
    flake.nixosModules.nix =
      {
        lib,
        config,
        modulesPath,
        pkgs,
        specialArgs,
        ...
      }:
      let
        cfg = config.nixos-config.nix.accessTokens;
        hasTokens = cfg != { };
        names = config.nixos-config.nix._accessTokenSecretNames;
        tokenLine = lib.concatStringsSep " " (
          lib.mapAttrsToList (site: _: "${site}=${config.sops.placeholder.${names.${site}}}") cfg
        );
      in
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

        options.nixos-config.nix._accessTokenSecretNames = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          internal = true;
          default = { };
          description = ''
            Resolved mapping of site -> sops placeholder key, set by the sops or
            clan backend companion module. Used to render the extra-access-tokens
            line; not intended to be set directly.
          '';
        };

        imports = [
          inputs.determinate.nixosModules.default
        ]
        # Backend selection lives here, outside config eval — driven purely by
        # specialArgs, exactly like clan-baseline is included.
        ++ [
          (
            if specialArgs ? clan-core then
              self.nixosModules.nix-access-tokens-clan
            else
              self.nixosModules.nix-access-tokens-sops
          )
        ];

        config = lib.mkMerge [
          {
            # nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

            nix = {
              package = lib.mkForce pkgs.nix; # from determinate-nix overlay
              settings = {
                sandbox = true;
                substituters = [ "https://cache.nixos.org" ];
              };
              extraOptions = ''
                experimental-features = nix-command flakes ca-derivations
              '';
            };

            users.groups.nix-access-tokens = { };

            nix.settings.trusted-users = [ "root" ];

            # nixpkgs.config = nixpkgsConfig;
          }
          # Shared rendering (both backends): sops-nix substitutes the placeholder
          # (clan-var or plain sops) into a tmpfs file readable by nix-access-tokens,
          # which the forgejo-runner DynamicUser belongs to for flake-input fetches.
          (lib.mkIf hasTokens {
            sops.templates."nix-access-tokens" = {
              content = "extra-access-tokens = ${tokenLine}\n";
              group = "nix-access-tokens";
              mode = "0440";
            };

            nix.extraOptions = ''
              !include ${config.sops.templates."nix-access-tokens".path}
            '';
          })
        ];
      };

    # Non-clan backend: a plain per-machine sops secret per site; the attrset value
    # is the sops secret name, used directly as the placeholder key.
    flake.nixosModules.nix-access-tokens-sops =
      { lib, config, ... }:
      let
        cfg = config.nixos-config.nix.accessTokens;
      in
      {
        key = "nixos-config.modules.nixos.nix-access-tokens-sops";
        config = {
          nixos-config.nix._accessTokenSecretNames = cfg;
          sops.secrets = lib.mapAttrs' (_site: secretName: {
            name = secretName;
            value = { };
          }) cfg;
        };
      };

    # Clan backend: one shared prompt clan-vars generator per site (single source
    # of truth across builders), modeled on clan.core.vars.generators.tailscale-admin.
    # The clan sops backend registers each secret as sops.secrets."vars/<gen>/token",
    # so that path is the placeholder key. The raw token file stays root-only 0400 —
    # only root renders the shared template.
    flake.nixosModules.nix-access-tokens-clan =
      { lib, config, ... }:
      let
        cfg = config.nixos-config.nix.accessTokens;
        genName = site: "nix-access-token-${lib.replaceStrings [ "." ] [ "-" ] site}";
      in
      {
        key = "nixos-config.modules.nixos.nix-access-tokens-clan";
        config = {
          nixos-config.nix._accessTokenSecretNames = lib.mapAttrs (
            site: _: "vars/${genName site}/token"
          ) cfg;
          clan.core.vars.generators = lib.mapAttrs' (
            site: _:
            lib.nameValuePair (genName site) {
              share = true;
              prompts.token.description = "nix access token for ${site} (e.g. GitHub PAT)";
              files.token = {
                secret = true;
                deploy = true;
              };
              script = ''
                tr -d '\n' < "$prompts/token" > "$out/token"
              '';
            }
          ) cfg;
        };
      };
  };
}
