{
  inputs,
  self,
  config,
  ...
}:
let
  flakeConfig = config;
  inventoryHostName = "niks3";
in
{
  flake-file.inputs.niks3 = {
    url = "github:Mic92/niks3";
    inputs.nixpkgs.follows = "nixpkgs-unstable";
    inputs.flake-parts.follows = "flake-parts";
    inputs.treefmt-nix.follows = "treefmt-nix";
  };

  clan.inventory.machines.niks3 = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
  };

  clan.machines.niks3 = {
    imports = [
      self.nixosModules.niks3-configuration
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
  };

  flake.nixosModules.niks3-configuration =
    {
      self',
      pkgs,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.niks3-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
        inputs.niks3.nixosModules.default
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

      clan.core.vars.generators.s3 = {
        prompts.access-key.description = "s3 key id";
        prompts.secret-key.description = "s3 secret key";
        files.access-key.secret = true;
        files.secret-key.secret = true;
        script = ''
          cat $prompts/access-key > $out/access-key
          cat $prompts/secret-key > $out/secret-key
        '';
      };

      clan.core.vars.generators.cache-key = {
        files.signing-key = { };
        files.public-key.secret = false;
        runtimeInputs = [
          pkgs.nix
        ];
        script = ''
          nix --extra-experimental-features nix-command \
            key generate-secret --key-name binarin-niks3-cache-1 > $out/signing-key
          nix --extra-experimental-features nix-command \
            key convert-secret-to-public < $out/signing-key > $out/public-key
        '';
      };

      clan.core.vars.generators.niks3-api-token = {
        files.api-token = { };
        runtimeInputs = [ pkgs.openssl ];
        script = ''
          openssl rand -base64 32 > $out/api-token
        '';
      };

      services.niks3 = {
        enable = false;
        httpAddr = "127.0.0.1:5751";

        s3 = {
          endpoint = "s3.lynx-lizard.ts.net";
          bucket = "niks3-storage";
          region = "garage";
          useSSL = true;
          accessKeyFile = config.clan.core.vars.generators.s3.files.access-key.path;
          secretKeyFile = config.clan.core.vars.generators.s3.files.secret-key.path;
        };

        # cacheUrl = "";

        apiTokenFile = config.clan.core.vars.generators.niks3-api-token.files.api-token.path;
        signKeyFiles = [
          config.clan.core.vars.generators.cache-key.file.signing-key.path
        ];
      };

      services.cloudflared = {
        enable = false;

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

      nixos-config.export-metrics.enable = true;
    };
}
