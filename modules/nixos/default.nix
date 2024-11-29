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

    users.users."root".openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMCVAKqmUdCkJ1gbi2ZA6vLnmf880U/9v5bfxhChapWB binarin@nixos"
    ];
  };
}
