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

        # Builder access stays restricted: no universal login keys and no
        # unrestricted default principal.
        swanLake.sshTrust.accounts.nix-remote-builder = {
          trustedLoginKeys = false;
          includeDefaultPrincipal = false;
          extraPrincipals =
            lib.forEach
              [
                "nix-remote-builder"
                "binarin"
                "root"
              ]
              (name: {
                inherit name;
                options = ''restrict,command="nix-daemon --stdio"'';
              });
        };
        roles.nix-remote-builder.schedulerPublicKeys = [ ];
      };
    };
}
