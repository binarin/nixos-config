{
  self,
  ...
}:
{
  flake.homeModules.wayland =
    {
      lib,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.home.wayland";

      config = lib.mkIf config.hostConfig.feature.wayland {
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

  flake.nixosModules.wayland =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.wayland";

      config.home-manager.sharedModules = [ self.homeModules.wayland ];
    };

  nixosSharedModules = [ self.nixosModules.wayland ];
}
