{
  self,
  inputs,
  lib,
  ...
}:
let
  nixpkgsConfig = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };
in
{
  flake-file.inputs = {
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/*";
  };

  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.emacs-overlay.overlays.default
          self.overlays.my-emacs
          self.overlays.my-google-chrome
        ];
        config = nixpkgsConfig;
      };
    };

  flake.nixosModules.nix =
    { config, ... }:
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
          nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

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

          nixpkgs.config = nixpkgsConfig;
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

}
