{ ... }:
{
  flake.homeModules.swaync =
    { pkgs, ... }:
    {

      home.packages = with pkgs; [
        swaynotificationcenter
      ];

      systemd.user.services.swaync = {
        Unit = {
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          Type = "simple";
          ExecStart = "${pkgs.swaynotificationcenter}/bin/swaync";
        };

        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };

    };
}
