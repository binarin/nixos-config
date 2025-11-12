{ inputs, ... }:
{
  flake.homeModules.wl-kbptr =
    { pkgs, config, ... }:
    {
      home.packages = [
        inputs.nixpkgs-unstable.legacyPackages."${pkgs.system}".wl-kbptr
      ];

      xdg.configFile."wl-kbptr/config".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/modules/binarin/wl-kbptr/config";
    };
}
