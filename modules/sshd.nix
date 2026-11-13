{ self, inputs, ... }:
{
  # Keys, trust roles and the principal policy come from common's ssh-trust
  # modules; this file only holds nixos-config's sshd defaults and the legacy
  # shared principals existing certificates rely on.
  flake.modules.generic.sshd-policy =
    { lib, ... }:
    {
      key = "nixos-config.modules.generic.sshd-policy";

      config = {
        services.openssh.settings.PermitRootLogin = lib.mkDefault "prohibit-password";
        swanLake.sshTrust.enable = true;
        swanLake.sshTrust.accounts.root.extraPrincipals = lib.mkDefault [ "root" ];
      };
    };

  flake.nixosModules.sshd =
    { ... }:
    {
      key = "nixos-config.modules.nixos.sshd";

      imports = [
        inputs.common.modules.nixos.ssh-trust
        self.modules.generic.sshd-policy
      ];

      config = {
        networking.firewall.allowedTCPPorts = [ 22 ];

        services.openssh.enable = true;
        services.openssh.authorizedKeysInHomedir = false;
      };
    };

  flake.systemModules.sshd =
    { lib, ... }:
    {
      key = "nixos-config.systemModules.sshd";

      imports = [
        inputs.common.modules.systemManager.ssh-trust
        self.modules.generic.sshd-policy
      ];

      config = {
        services.openssh.enable = true;
        services.openssh.openFirewall = false;
        services.openssh.authorizedKeysInHomedir = false;

        systemd.services."ssh-system-manager".aliases = lib.mkForce [ ];
      };
    };
}
