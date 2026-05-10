{
  inputs,
  self,
  config,
  lib,
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

  flake.nixosConfigurations.niks3 = lib.mkForce (
    self.clan.nixosConfigurations.niks3.extendModules {
      specialArgs.inventoryHostName = "niks3";
    }
  );

  clan.inventory.machines.niks3 = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
  };

  clan.machines.niks3 = {
    imports = [
      self.nixosModules.niks3-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
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

      clan.core.vars.generators.s3 = {
        prompts.access-key.description = "s3 key id";
        prompts.secret-key.description = "s3 secret key";
        files.access-key = {
          owner = config.services.niks3.user;
          group = config.services.niks3.group;
        };
        files.secret-key = {
          owner = config.services.niks3.user;
          group = config.services.niks3.group;
        };
        script = ''
          cat $prompts/access-key > $out/access-key
          cat $prompts/secret-key > $out/secret-key
        '';
      };

      clan.core.vars.generators.cache-key = {
        files.signing-key = {
          owner = config.services.niks3.user;
          group = config.services.niks3.group;
        };
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
        files.api-token = {
          owner = config.services.niks3.user;
          group = config.services.niks3.group;
        };
        runtimeInputs = [ pkgs.openssl ];
        script = ''
          openssl rand -base64 32 > $out/api-token
        '';
      };

      services.niks3 = {
        enable = true;
        httpAddr = "127.0.0.1:5751";

        oidc.providers.gitlab = {
          issuer = "https://forgejo.lynx-lizard.ts.net";
          audience = "https://niks3.lynx-lizard.ts.net";
          boundClaims = {
            repository_owner = [ "binarin" ];
          };
        };

        s3 = {
          endpoint = "s3.lynx-lizard.ts.net";
          bucket = "niks3-storage";
          region = "garage";
          useSSL = true;
          accessKeyFile = config.clan.core.vars.generators.s3.files.access-key.path;
          secretKeyFile = config.clan.core.vars.generators.s3.files.secret-key.path;
        };

        cacheUrl = "http://niks3-storage.home.binarin.info";

        apiTokenFile = config.clan.core.vars.generators.niks3-api-token.files.api-token.path;
        signKeyFiles = [
          config.clan.core.vars.generators.cache-key.files.signing-key.path
        ];
      };

      services.tailscale.serve.enable = true;
      services.tailscale.serve.configs.niks3 = {
        target = "5751";
      };

      nixos-config.export-metrics.enable = true;
    };
}
