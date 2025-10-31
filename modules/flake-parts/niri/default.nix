{self, ...}: {
  flake.nixosModules.niri = {pkgs, lib, config, ...}: let
    # wrapper script for `binPath` since the option type is `path`
    niriSession = lib.getExe (pkgs.writeShellScriptBin "niriSession" ''
        ${lib.getExe config.programs.niri.package } --session
      '');
  in {

    environment.systemPackages = with pkgs; [
      # Things used by the default config
      alacritty
      fuzzel

      # niri binary itself
      niri

      # automatically started if installed
      xwayland-satellite
    ];

    programs.uwsm = {
      enable = true;
      waylandCompositors = {
        niri = {
          prettyName = "niri";
          comment = "niri compositor managed by UWSM";
          binPath = niriSession;
        };
      };
    };
    home-manager.defaultImports = [ self.homeModules.niri ];
  };

  flake.homeModules.niri = {pkgs, lib, config, ...}: {
    xdg.configFile."niri/config.kdl".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/modules/flake-parts/niri/config.kdl";
  };
}
