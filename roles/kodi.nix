{...}:
{
  time.timeZone = "Europe/Moscow";
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";

  nixpkgs.config.allowUnfree = true;
  users.extraUsers.binarin = {
    isNormalUser = true;
    uid = 1000;
  };
  services.xserver.desktopManager.kodi.enable = true;
  services.xserver.enable = true;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.greeter.enable = false;
  services.xserver.displayManager.lightdm.autoLogin = {
    enable = true;
    user = "binarin";
    timeout = 0;
  };

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 1814 8080 ];
  networking.firewall.allowedUDPPorts = [ 1900 6178 9777 ];
}
