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

  flake.overlays.my-emacs =
    final: prev:
    let
      emacsMaker =
        base:
        final.callPackage ../../packages/my-emacs {
          emacsBasePackage = base;
          flakyConfigDir =
            { emacsWithPackages, ... }:
            final.stdenv.mkDerivation {
              name = "user-init-dir";
              src = selfLib.dir "emacs";
              buildPhase = ''
                mkdir "$out"
                ${final.lib.getExe final.rsync} -a "$src/" "$out/"
                chmod -R u+rwX "$out"
                ${final.lib.getExe emacsWithPackages} --init-directory "$out" --batch --eval \
                   '(let ((no-native-compile t))
                      (prepare-user-lisp nil "'"$out"'/user-lisp/.user-lisp-autoloads.el" t))'
              '';
            };
        };
    in
    {
      my-emacs = emacsMaker final.emacs-git-nox;
      my-emacs-nox = emacsMaker final.emacs-git-nox;
      my-emacs-pgtk = emacsMaker final.emacs-git-pgtk;
    };

  perSystem =
    {
      self',
      pkgs,
      lib,
      ...
    }:
    {
      devShells.emacs-test = pkgs.mkShell {
        name = "emacs-test-devshell";
        packages = with pkgs; [
          coreutils
          bash
          bubblewrap
          rsync
          gnugrep
          my-emacs-nox
        ];
        shellHook = ''
          export IN_TEST_SHELL=1
          export EMACS_PGTK="${lib.getExe pkgs.my-emacs-pgtk}"
        '';
      };
    };

  flake.nixosModules.emacs =
    {
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.emacs";

      nixpkgs.overlays = [
        inputs.emacs-overlay.overlays.default
        self.overlays.my-emacs
        self.overlays.sicstus-manual
        self.overlays.waybar-org-clock
      ];

      environment.systemPackages = [
        pkgs.my-emacs
      ];
    };

  flake.homeModules.emacs =
    {
      config,
      pkgs,
      lib,
      osConfig,
      ...
    }:
    {
      key = "nixos-config.modules.home.emacs";

      imports = [
        self.homeModules.impermanence
      ];

      config = lib.mkMerge [
        {

          home.file.".config/emacs/snippets".source = selfLib.dir "yasnippets";

          programs.emacs = {
            enable = lib.mkForce false;
            package = lib.mkOverride 90 (
              pkgs.my-emacs.override {
                emacsBasePackage = pkgs.emacs-git-nox.override {
                  withNS = false;
                  withX = false;
                  withGTK3 = false;
                  withWebP = false;
                };
              }
            );
          };

          home.sessionVariables.EDITOR = "emacs-as-console-editor";

          home.packages = [
            config.programs.emacs.package
            (pkgs.writeShellScriptBin "emacs-as-console-editor" ''
              exec emacsclient -a 'emacs -nw' -nw "$@"
            '')
          ];

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
            pkgs.my-emacs.override {
              emacsBasePackage = pkgs.emacs-git-pgtk;
              extraPackages = with pkgs; [
                direnv
                sicstus-manual
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
