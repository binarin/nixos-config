{ self, ... }:
{
  flake.nixosModules.niri =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
      # wrapper script for `binPath` since the option type is `path`
      niriSession = lib.getExe (
        pkgs.writeShellScriptBin "niriSession" ''
          ${lib.getExe config.programs.niri.package} --session
        ''
      );
    in
    {
      key = "nixos-config.programs.niri";

      imports = [
        self.nixosModules.gui
      ];

      environment.systemPackages = with pkgs; [
        # Things used by the default config
        alacritty
        fuzzel

        # niri binary itself, for RPC calls
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
    };

  flake.homeModules.niri =
    {
      lib,
      config,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.home.niri";

      imports = [
        self.homeModules.wayland
        self.homeModules.fuzzel
        self.homeModules.waybar
      ];

      home.packages = [
        self.packages."${pkgs.stdenv.system}".sshmenu
      ];

      xdg.configFile."niri/config.kdl".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/modules/flake-parts/niri/config.kdl";
    };
}
