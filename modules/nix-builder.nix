{
  self,
  inputs,
  ...
}:
{
  flake.nixosModules.nix-builder =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.nix-builder";
      imports = [
        "${inputs.srvos}/nixos/roles/nix-remote-builder.nix"

        self.nixosModules.impure-nix-setup
      ];

      config = {
        nixos-config.nix.accessTokens = {
          "github.com" = "extra-access-tokens/github.com";
        };

        nixos-config.export-metrics.enable = true;

        nix.settings.system-features = [
          "big-parallel"
        ];

        nix.extraOptions = ''
          build-dir = /nix/build
        '';

        users.users.nix-remote-builder.openssh.authorizedPrincipals = lib.forEach [
          "nix-remote-builder"
          "binarin"
          "root"
        ] (k: ''restrict,command="nix-daemon --stdio" ${k}'');
        roles.nix-remote-builder.schedulerPublicKeys = [ ];
      };
    };
}
