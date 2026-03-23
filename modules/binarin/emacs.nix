{
  self,
  inputs,
  config,
  ...
}:
let
  selfLib = self.lib.self;
in
{
  flake-file.inputs = {
    emacs-overlay.url = "emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs";
  };

  flake.nixosModules.emacs =
    {
      pkgs,
      self',
      ...
    }:
    {
      key = "nixos-config.modules.nixos.emacs";

      nixpkgs.overlays = [
        inputs.emacs-overlay.overlays.default
      ];

      environment.systemPackages = with pkgs; [
        self'.packages.emacs-nox
      ];
    };

  flake.homeModules.emacs =
    {
      config,
      pkgs,
      lib,
      osConfig,
      self',
      ...
    }:
    {
      key = "nixos-config.modules.home.emacs";

      config = lib.mkMerge [
        {
          home.file.".config/emacs/snippets".source = selfLib.dir "yasnippets";

          programs.emacs = {
            enable = lib.mkForce false;
            package = lib.mkOverride 90 self'.packages.emacs-nox;
          };

          impermanence.persist-directories = [
            ".local/state/emacs-clean"
          ];

          impermanence.local-directories = [
            ".cache/emacs-clean"
          ];
        }

        (lib.mkIf osConfig.services.graphical-desktop.enable {
          xdg.dataFile."applications/org-protocol.desktop".source = selfLib.file "org-protocol.desktop";

          xdg.configFile."autostart/emacs.desktop".source =
            "${config.programs.emacs.package}/share/applications/emacs.desktop";

          xdg.mimeApps.defaultApplications = lib.mkIf pkgs.stdenv.isLinux {
            "x-scheme-handler/org-protocol" = "org-protocol.desktop";
          };

          xdg.dataFile."icons/emacs/org.svg".source = selfLib.file "org.svg";

          programs.emacs.package = lib.mkForce (
            self'.packages.emacs-pgtk.override {
              extraPackages = with pkgs; [
                emacs-all-the-icons-fonts
                ghostscript
                wtype
              ];
            }
          );
        })
      ];
    };
}
