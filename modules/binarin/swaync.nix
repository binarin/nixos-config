{ ... }:
{
  flake.homeModules.swaync =
    { ... }:
    {
      key = "nixos-config.modules.home.swaync";
      services.swaync = {
        enable = true;
        settings = {
          positionX = "right";
          positionY = "bottom";
        };
      };
    };
}
