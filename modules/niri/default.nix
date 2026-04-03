{ self, inputs, ... }:
let
  selfLib = self.lib.self;
  niriCargo = builtins.fromTOML (selfLib.read' "packages/niri-dynamic-keybindings/Cargo.toml");
in
{
  flake-file.inputs.niri = {
    url = "github:niri-wm/niri?rev=${niriCargo.dependencies.niri-ipc.rev}";
    inputs.nixpkgs.follows = "nixpkgs";
  };

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

      nixpkgs.overlays = [
        inputs.niri.overlays.default
        self.overlays.waybar-org-clock
      ];

      programs.niri.enable = true;

      environment.systemPackages = with pkgs; [
        # Things used by the default config
        alacritty
        fuzzel

        # automatically started if installed
        # xwayland-satellite XXX leak debugging
      ];

      services.gnome.gcr-ssh-agent.enable = false;

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
      lib,
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
        self.homeModules.niri-dynamic-keybindings
      ];

      config = {
        home.packages = [
          self.packages."${pkgs.stdenv.hostPlatform.system}".sshmenu
        ];

        # xdg.configFile."niri/config.kdl".source =
        #   config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/modules/niri/config.kdl";

        xdg.configFile."niri/config.kdl".source = selfLib.file' "modules/niri/config.kdl";
        xdg.configFile."niri/dynamic-outputs.kdl".source = selfLib.file' "modules/niri/dynamic-outputs.kdl";

        home.activation.niri-config-materialize = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
          run touch ~/.config/niri/dynamic-binds.kdl
          if [[ -d $HOME/personal-workspace/nixos-config ]]; then
            for niri_cfg in config.kdl dynamic-outputs.kdl; do
              if [[ -L $HOME/.config/niri/$niri_cfg ]]; then
                run ln -sf $HOME/personal-workspace/nixos-config/modules/niri/$niri_cfg $HOME/.config/niri/$niri_cfg
              fi
            done
          fi
        '';

        xdg.portal.extraPortals = [
          pkgs.kdePackages.kwallet
          pkgs.kdePackages.xdg-desktop-portal-kde
          pkgs.kdePackages.polkit-kde-agent-1
        ];
      };
    };
}
