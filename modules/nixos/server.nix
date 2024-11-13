{ flake, config, lib, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  config = lib.mkIf config.hostConfig.feature.server {
    time.timeZone = "Europe/Amsterdam";
    services.openssh.enable = true;
    services.openssh.settings.PermitRootLogin = "yes";
    services.fail2ban.enable = true;
    networking.firewall.allowedTCPPorts = [
      22
    ];

    nixpkgs.config = {
      allowUnfree = true;
      oraclejdk.accept_license = true;
    };

    networking.firewall.enable = true;

    environment.enableAllTerminfo = true;

    users.users."root".openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMCVAKqmUdCkJ1gbi2ZA6vLnmf880U/9v5bfxhChapWB binarin@nixos"
    ];
  };
}
