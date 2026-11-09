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
  imports = [
    inputs.clan-core.flakeModules.default
  ];

  clan = {
    meta.name = "binarin-nixos-config";
    meta.domain = "clan.binarin.info";
    pkgsForSystem = system: self.lib.pkgsFor system;
  };

  flake.nixosModules.clan-tailscale =
    {
      self',
      lib,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.clan-tailscale";
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

      clan.core.vars.generators.tailscale-auth = lib.mkIf config.services.tailscale.enable {
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
           --no-ephemeral \
           --expiry 604800 \
           > $out/tailscale-auth
        '';
      };

      services.tailscale.authKeyFile =
        config.clan.core.vars.generators.tailscale-auth.files.tailscale-auth.path;
    };

  flake.nixosModules.clan-baseline =
    {
      self',
      inventoryHostName,
      config,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.clan-baseline";
      imports = [
        self.nixosModules.clan-hostId
        self.nixosModules.clan-hosts
        self.nixosModules.clan-tailscale
      ];

      hardware.facter.report.system = lib.mkForce false;

    };

  flake.nixosModules.clan-hosts =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.clan-hosts";
      config = {
        networking.hosts =
          with lib;
          pipe config.clanConfig.machines [
            attrNames
            (map (
              name:
              lib.optionalAttrs
                (name != config.networking.hostName && (flakeConfig.inventory.ipAllocation."${name}" or { }) ? home)
                {
                  "${flakeConfig.inventory.ipAllocation."${name}".home.primary.address}" = [
                    "${name}.${config.clan.core.settings.domain}"
                  ];
                }
            ))
            mergeAttrsList
          ];
      };
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
          let
            v = builtins.tryEval (lib.trim config.clan.core.vars.generators.hostId.files.hostId.value);
          in
          if v.success then v.value else "00000000"
        );
      };
    };

  flake.nixosModules.clan-smtp =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.clan-smtp";
      config = {

        users.groups.smtp-password = { };

        clan.core.vars.generators.smtp = {
          share = true;
          prompts.host.description = "smtp host";
          prompts.port.description = "smtp port";
          prompts.user.description = "smtp user";
          prompts.password.description = "smtp password";
          files.host = {
            group = "smtp-password";
            mode = "0640";
          };
          files.port = {
            group = "smtp-password";
            mode = "0640";
          };
          files.user = {
            group = "smtp-password";
            mode = "0640";
          };
          files.password = {
            group = "smtp-password";
            mode = "0640";
          };
          script = ''
            for f in host port user password; do
              cat $prompts/$f > $out/$f
            done
          '';
        };
      };
    };

}
