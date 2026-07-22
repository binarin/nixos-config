{ ... }:
{
  flake.homeModules.wprintidle =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.wprintidle";

      config = {
        home.packages = [ pkgs.wprintidle-c ];

        systemd.user.services.wprintidle-c = {
          Unit = {
            Description = "wprintidle-c Wayland idle tracker";
            PartOf = [ "graphical-session.target" ];
            After = [ "graphical-session.target" ];
            Wants = [ "graphical-session.target" ];
          };

          Service = {
            ExecStart = "${pkgs.wprintidle-c}/bin/wprintidle-c-daemon 1000";
            Restart = "on-failure";
            RestartSec = 5;
          };

          Install = {
            WantedBy = [ "graphical-session.target" ];
          };
        };
      };
    };
}
