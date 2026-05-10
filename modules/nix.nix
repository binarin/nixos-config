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
    ];
  };

  defaultOverlays = [
    inputs.emacs-overlay.overlays.default
    inputs.nix-ai-tools.overlays.shared-nixpkgs
    inputs.niri.overlays.default

    # devenv 2.1
    inputs.devenv.overlays.default

    # hyprland 0.55
    inputs.hyprland.overlays.hyprland-packages
    inputs.hyprland.overlays.hyprland-extras
    inputs.hyprland-contrib.overlays.default

    self.overlays.sicstus-manual
    self.overlays.slack
    self.overlays.my-emacs
    self.overlays.my-google-chrome
    self.overlays.waybar-org-clock
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
      overlays = defaultOverlays ++ extraOverlays;
    };

in
{
  config = {
    flake-file.inputs = {
      determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/*";
    };

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
          (_final: _prev: {
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

    flake.nixosModules.nix =
      {
        lib,
        config,
        modulesPath,
        ...
      }:
      let
        cfg = config.nixos-config.nix.accessTokens;
        hasTokens = cfg != { };
        tokenLine = lib.concatStringsSep " " (
          lib.mapAttrsToList (site: secretName: "${site}=${config.sops.placeholder.${secretName}}") cfg
        );
      in
      {
        key = "nixos-config.modules.nixos.nix";

        options.nixos-config.nix.accessTokens = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = { };
          description = ''
            Attrset mapping hostnames to sops secret names for nix access tokens.
            Example: { "github.com" = "extra-access-tokens/github.com"; }
          '';
        };

        config = lib.mkMerge [
          {
            # nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

            nix = {
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
          (lib.mkIf hasTokens {
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
          })
        ];
      };
  };
}
