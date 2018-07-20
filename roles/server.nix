{...}:
{
  imports = [
    ../packages/standard-linux-tools.nix
    ../packages/nixpkgs-from-submodule.nix
    # ../modules/outgoing-email.nix
  ];

  time.timeZone = "Europe/Amsterdam";
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";
  services.fail2ban.enable = true;
  networking.firewall.allowedTCPPorts = [
    22
  ];

  nixpkgs.config.allowUnfree = true;

  networking.firewall.enable = true;
}
