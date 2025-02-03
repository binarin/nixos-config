{
  flake,
  config,
  lib,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  config = lib.mkIf config.hostConfig.feature.server {
    # services.fail2ban.enable = true;
    # services.openssh.enable = true;
  };
}
