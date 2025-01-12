{
  flake,
  pkgs,
  lib,
  config,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;
in
{
  imports = [
    self.sharedModules.default
    inputs.arion.nixosModules.arion
  ] ++ lib.attrValues (lib.removeAttrs self.nixosModules [ "default" ]);

  options = { };

  config = {
    security.pam.services.login.rules.session.env.args = [ "debug" ];
    security.pam.services.login.rules.session.systemd.args = [ "debug" ];
    security.pam.services.login.rules.session.kwallet.args = [ "debug" ];

    system.stateVersion = lib.mkDefault "24.05";
    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    system.switch.enableNg = lib.mkDefault true;
    system.switch.enable = lib.mkDefault false;

    services.dbus.implementation = "broker";

    services.openssh.settings.TrustedUserCaKeys = "/etc/ssh/trusted_user_ca_keys";
    services.openssh.authorizedKeysInHomedir = false;

    environment.etc."ssh/trusted_user_ca_keys".text = lib.concatStringsSep "\n" ( config.lib.publicKeys.secureWithTag "user-ca");

    users.users = lib.genAttrs (["root"] ++ config.hostConfig.managedUsers) (user: {
      openssh.authorizedKeys.keys = config.lib.publicKeys.ssh.secureForUser user;
    });

  };
}
