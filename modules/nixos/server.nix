{
  config,
  lib,
  ...
}:
{
  config = lib.mkIf config.hostConfig.feature.server {
    # services.fail2ban.enable = true;
    # services.openssh.enable = true;
  };
}
