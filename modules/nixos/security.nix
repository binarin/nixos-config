{ config, ... }:
{
  options = {
  };

  config = {
    security.polkit.enable = true;

    networking.firewall.enable = true;

    security.sudo = {
      enable = true;
      wheelNeedsPassword = false;
    };

    security.pam.loginLimits = [
      {
        domain = "*";
        type = "-";
        item = "nofile";
        value = "131072";
      }
    ];
  };
}
