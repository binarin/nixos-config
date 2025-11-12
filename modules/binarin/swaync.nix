{ ... }:
{
  flake.homeModules.swaync =
    { pkgs, ... }:
    {
      services.swaync = {
        enable = true;
        settings = {
          positionX = "right";
          positionY = "bottom";
        };
      };
    };
}
