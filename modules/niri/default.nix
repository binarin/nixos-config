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
      key = "nixos-config.modules.nixos.niri";

      imports = [
        self.nixosModules.gui
        self.nixosModules.wayland
        self.nixosModules.swayidle
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
      config,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.home.niri";

      imports = [
        self.homeModules.wayland
        self.homeModules.fuzzel
        self.homeModules.waybar
        self.homeModules.swaync
        self.homeModules.wl-kbptr
        self.homeModules.swayidle
      ];

      config = {
        home.packages = [
          self.packages."${pkgs.stdenv.hostPlatform.system}".sshmenu
        ];

        xdg.configFile."niri/config.kdl".source =
          config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/modules/niri/config.kdl";

        # XXX
        # home.activation.niri-layout-binds = lib.hm.dag.entryAfter ["writeBoundary"] ''
        #   run touch ~/.config/niri/layout-binds.kdl
        # '';

        xdg.portal.extraPortals = [
          pkgs.kdePackages.kwallet
          pkgs.kdePackages.xdg-desktop-portal-kde
        ];

      };
    };
}
