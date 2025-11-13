{
  ...
}:
{
  flake.nixosModules.wayland = { ... }: {
    programs.dconf.enable = true;
  };


  flake.homeModules.wayland =
    {
      config,
      ...
    }:
    {
      key = "nixos-config.modules.home.wayland";

      config = {
        home.sessionVariables = {
          MOZ_ENABLE_WAYLAND = "1";
          NIXOS_OZONE_WL = "1";
          SDL_VIDEODRIVER = "wayland";

          # needs qt5.qtwayland in systemPackages
          QT_QPA_PLATFORM = "wayland";
          QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
        };
      };
    };
}
