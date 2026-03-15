{
  lib,
  inputs,
  self,
  config,
  ...
}:
let
  flakeConfig = config;
  selfLib = self.lib.self;
in
{
  flake-file.inputs.clan-core = {
    # url = "https://git.clan.lol/clan/clan-core/archive/25.11.tar.gz";
    # url = "git+file:/home/binarin/personal-workspace/nix/clan-core";
    url = "git+https://forgejo.lynx-lizard.ts.net/binarin/clan-core?ref=25.11";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.disko.follows = "disko";
    inputs.flake-parts.follows = "flake-parts";
    inputs.sops-nix.follows = "sops-nix";
    inputs.treefmt-nix.follows = "treefmt-nix";
  };

  imports = [
    inputs.clan-core.flakeModules.default
  ];

  clan = {
    meta.name = "binarin-nixos-config";
    meta.domain = "clan.binarin.info";
  };

  flake.nixosModules.clan-baseline =
    {
      self',
      inventoryHostName,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.clan-baseline";
      imports = [
        self.nixosModules.clan-hostId
      ];

      clan.core.vars.generators.tailscale-admin = {
        share = true;
        prompts.oauth-client-id = {
          description = "OAuth client id for machine join tokens generation";
        };
        prompts.oauth-client-secret = {
          description = "OAuth client secret for machine join tokens generation";
        };
        files.oauth-client-id = {
          secret = true;
          deploy = false;
        };
        files.oauth-client-secret = {
          secret = true;
          deploy = false;
        };
        script = ''
          cat $prompts/oauth-client-id > $out/oauth-client-id
          cat $prompts/oauth-client-secret > $out/oauth-client-secret
        '';
      };

      clan.core.vars.generators.tailscale-auth = {
        files.tailscale-auth = {
          secret = true;
        };
        runtimeInputs = [ self'.packages.ncf ];
        dependencies = [ "tailscale-admin" ];
        script = ''
          ncf ts auth-key \
          --client-id-file $in/tailscale-admin/oauth-client-id \
          --client-secret-file $in/tailscale-admin/oauth-client-secret \
           --reusable \
           --expiry 604800 \
           > $out/tailscale-auth
        '';
      };

      services.tailscale.authKeyFile =
        config.clan.core.vars.generators.tailscale-auth.files.tailscale-auth.path;
    };

  flake.nixosModules.clan-hostId =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.clan-hostId";
      config = {
        clan.core.vars.generators.hostId = {
          files.hostId.secret = false;
          runtimeInputs = with pkgs; [
            openssl
          ];
          script = ''
            openssl rand -hex 4 > $out/hostId
          '';
        };
        networking.hostId = lib.mkForce (
          lib.trim config.clan.core.vars.generators.hostId.files.hostId.value
        );
      };
    };
}
