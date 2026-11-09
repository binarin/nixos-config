{ self, inputs, ... }:
{
  flake.homeModules.wl-kbptr =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.home.wl-kbptr";
      home.packages = [
        pkgs.wl-kbptr
      ];

      home.localCheckoutLinks.files."${lib.removePrefix "${config.home.homeDirectory}/" config.xdg.configHome}/wl-kbptr/config" =
        {
          checkout = "nixos-config";
          source = "modules/binarin/wl-kbptr/config";
          fallback = self.lib.self.file' "modules/binarin/wl-kbptr/config";
        };
    };
}
