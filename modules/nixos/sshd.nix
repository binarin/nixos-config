{ flake, pkgs, lib, config, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  config = {
    networking.firewall.allowedTCPPorts = [
      22
    ];

    services.openssh.enable = true;
    services.openssh.authorizedKeysInHomedir = false;
    services.openssh.settings.PermitRootLogin = lib.mkDefault "prohibit-password";
    services.openssh.settings.TrustedUserCaKeys = "/etc/ssh/trusted_user_ca_keys";
    services.openssh.settings.AuthorizedPrincipalsFile = "/etc/ssh/authorized_principals.d/%u";

    environment.etc."ssh/trusted_user_ca_keys".text = lib.concatStringsSep "\n" ( config.lib.publicKeys.secureWithTag "user-ca");

    users.users.root.openssh = {
      authorizedKeys.keys = lib.mkDefault (config.lib.publicKeys.ssh.secureForUser "root");
      authorizedPrincipals = lib.mkDefault ["root"];
    };
  };
}
