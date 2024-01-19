{...}:
{
  imports = [
    ../packages/standard-linux-tools.nix
    ../packages/use-my-overlays.nix
    # ../modules/outgoing-email.nix
  ];

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
}
