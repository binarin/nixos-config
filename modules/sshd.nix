{ self, ... }:
{
  flake.modules.generic.sshd-policy =
    { lib, config, ... }:
    {
      key = "nixos-config.modules.generic.sshd-policy";

      imports = [
        self.modules.generic.public-keys
      ];

      config = {
        services.openssh.settings.PermitRootLogin = lib.mkDefault "prohibit-password";
        services.openssh.settings.TrustedUserCaKeys = "/etc/ssh/trusted_user_ca_keys";
        services.openssh.settings.AuthorizedPrincipalsFile = "/etc/ssh/authorized_principals.d/%u";

        environment.etc."ssh/trusted_user_ca_keys".text = lib.concatStringsSep "\n" (
          config.lib.publicKeys.secureWithTag "user-ca"
        );
      };
    };

  flake.nixosModules.sshd =
    { lib, config, ... }:
    {
      key = "nixos-config.modules.nixos.sshd";

      imports = [
        self.modules.generic.sshd-policy
      ];

      config = {
        networking.firewall.allowedTCPPorts = [ 22 ];

        services.openssh.enable = true;
        services.openssh.authorizedKeysInHomedir = false;

        users.users.root.openssh = {
          authorizedKeys.keys = lib.mkDefault (config.lib.publicKeys.ssh.secureForUser "root");
          authorizedPrincipals = lib.mkDefault [ "root" ];
        };
      };
    };

  flake.systemModules.sshd =
    { lib, config, ... }:
    let
      cfg = config.services.openssh;
    in
    {
      key = "nixos-config.systemModules.sshd";

      imports = [
        self.modules.generic.sshd-policy
      ];

      options.services.openssh.managedUsers = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "root" ];
        description = "Users whose SSH authorized keys and principals are populated from public-keys.";
        example = [ "root" "allebedev" ];
      };

      config = {
        services.openssh.enable = true;
        services.openssh.openFirewall = false;
        services.openssh.authorizedKeysInHomedir = false;

        systemd.services."ssh-system-manager".aliases = lib.mkForce [ ];

        users.users = lib.listToAttrs (
          map (user: lib.nameValuePair user {
            openssh.authorizedKeys.keys = config.lib.publicKeys.ssh.secureForUser user;
            openssh.authorizedPrincipals = lib.optional (user == "root") "root";
          }) cfg.managedUsers
        );
      };
    };
}
