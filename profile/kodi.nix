{pkgs, ...}:
let
  hosts = import ../nixops/personal-hosts.nix;
in {
  time.timeZone = "Europe/Amsterdam";
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";

  nixpkgs.config.allowUnfree = true;

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

  services.wakeonlan.interfaces = [
    {
      interface = hosts.kodi.lan.iface;
      method = "magicpacket";
    }
  ];

  systemd.services.turn-tv-on = let
    script = pkgs.writeScript "turn-tv-on" ''
      #!${pkgs.bash}/bin/bash
      ${pkgs.coreutils}/bin/stty -F /dev/ttyUSB0 9600
      sleep 2
      echo -ne "ka 01 01\r\n" > /dev/ttyUSB0
      sleep 10
      echo -ne "xb 01 92\r\n" > /dev/ttyUSB0
      sleep 10
      echo -ne "xb 01 92\r\n" > /dev/ttyUSB0
    '';
  in {
    description = "Turns on LG TV (fuck you NVidia for absent HDMI CEC support)";
    wantedBy = [ "multi-user.target" "sleep.target" ];
    after = [ "sleep.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = script;
      TimeoutSec = 0;
      StandardOutput = "syslog";
    };
  };
}
