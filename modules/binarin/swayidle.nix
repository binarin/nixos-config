{ self, ... }:
{
  flake.nixosModules.swayidle =
    { ... }:
    {
      security.pam.services.swaylock = { };
    };

  flake.homeModules.swayidle =
    { lib, pkgs, ... }:
    {
      imports = [
        self.homeModules.stylix
      ];

      stylix.targets.swaylock.enable = true;
      programs.swaylock.enable = true;

      services.swayidle = {
        enable = true;
        events = [
          {
            event = "before-sleep";
            command = "${lib.getExe' pkgs.systemd "loginctl"} lock-session";
          }
          {
            event = "after-resume";
            command = "${lib.getExe pkgs.niri} msg action power-on-monitors";
          }
          {
            event = "lock";
            command = "${lib.getExe pkgs.swaylock} -fF";
          }
        ];
        timeouts = [
          {
            timeout = 180;
            command = "${lib.getExe pkgs.brightnessctl} -s s 10%";
            resumeCommand = "${lib.getExe pkgs.brightnessctl} -r";
          }
          {
            timeout = 300;
            command = "${lib.getExe' pkgs.systemd "loginctl"} lock-session";
          }
          {
            timeout = 330;
            command = "${lib.getExe pkgs.niri} msg action power-off-monitors";
          }
        ];
      };

    };

}
