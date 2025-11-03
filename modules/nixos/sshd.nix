{ flake, pkgs, lib, config, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;
in
{
  config = {
    services.openssh.enable = true;
    services.openssh.authorizedKeysInHomedir = false;
    services.openssh.settings.PermitRootLogin = lib.mkDefault "prohibit-password";
    services.openssh.settings.TrustedUserCaKeys = "/etc/ssh/trusted_user_ca_keys";

    environment.etc."ssh/trusted_user_ca_keys".text = lib.concatStringsSep "\n" ( config.lib.publicKeys.secureWithTag "user-ca");

    users.users.root.openssh = {
      authorizedKeys.keys = config.lib.publicKeys.ssh.secureForUser "root";
      authorizedPrincipals = ["root"];
    };
  };
}
