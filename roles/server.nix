{...}:
{
  imports = [
    ../packages/standard-linux-tools.nix
  ];

  time.timeZone = "Europe/Moscow";
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";

  nixpkgs.config.allowUnfree = true;
  users.extraUsers.binarin = {
    isNormalUser = true;
    uid = 1000;
  };

  networking.firewall.enable = true;
}
