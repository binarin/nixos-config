{ ... }:
{
  flake.homeModules.swaync =
    { ... }:
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
