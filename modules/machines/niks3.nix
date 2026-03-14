{ self, config, ... }:
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

  flake.deploy.nodes.niks3 = {
    hostname = "claude-nixos-config";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.claude-nixos-config;
    };
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
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
      ];

      _module.args.self' = {
        packages = self.packages."${pkgs.stdenv.hostPlatform.system}";
      };

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

      nixos-config.export-metrics.enable = true;
    };
}
