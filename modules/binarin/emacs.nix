{
  self,
  inputs,
  lib,
  config,
  ...
}:
let
  selfLib = self.lib.self;
in
{
  flake.overlays.my-emacs =
    final: prev:
    let
      emacsMaker =
        base:
        final.callPackage ../../packages/my-emacs {
          emacsBasePackage = base;
          bleedingEmacsPackages = final.bleeding.emacsPackages;
          emacsPackagesFn = epkgs: [
            (epkgs.melpaBuild {
              pname = "niri-rpc";
              version = "0.0.0";
              src = inputs.emacs-niri-awareness;
            })
            (epkgs.melpaBuild {
              pname = "tramp-rpc";
              version = "0.10.0";
              src = inputs.emacs-tramp-rpc;
              packageRequires = with epkgs; [
                msgpack
              ];
            })
          ];
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
    (lib.genAttrs [ "emacs-git" "emacs-git-nox" "emacs-git-pgtk" ] (
      name:
      prev.${name}.overrideAttrs (old: {
        # The common overlay owns patch selection. Fail if upstream equivalents regress.
        postPatch = old.postPatch + ''
          grep -Fq 'Lisp_Object css = make_formatted_string' src/image.c
          grep -Fq 'SBYTES (css), NULL);' src/image.c
          sed -n '/DEFUN ("intern",/,/^}/p' src/lread.c | grep -Fq 'tem = oblookup (obarray,'
          grep -Fq "(if (fboundp 'trusted-content-p) (trusted-content-p) t)" lisp/progmodes/flymake.el
          grep -Fq "(function-get backend 'flymake-always-safe)" lisp/progmodes/flymake.el
          grep -Fq '(rx bos (literal (tramp-build-prefix-format)))' lisp/net/tramp.el
          grep -Fq 'Not a valid user name:' lisp/net/tramp.el
          grep -Fq 'Not a valid host name:' lisp/net/tramp.el
          grep -Fq '(defsubst tramp-get-connection-local-criteria (vec)' lisp/net/tramp.el
          grep -Fq '(tramp-allow-double-percent . t)' lisp/net/tramp-archive.el
        '';
      })
    ))
    // {
      my-emacs = emacsMaker final.emacs-git-nox;
      my-emacs-nox = emacsMaker final.emacs-git-nox;
      my-emacs-pgtk = emacsMaker final.emacs-git-pgtk;
      tramp-rpc-server = final.callPackage "${inputs.emacs-tramp-rpc}/default.nix" { };
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

      # nixpkgs.overlays = [
      #   inputs.emacs-overlay.overlays.default
      #   self.overlays.my-emacs
      #   self.overlays.sicstus-manual
      #   self.overlays.waybar-org-clock
      # ];

      environment.systemPackages = with pkgs; [
        my-emacs
        tramp-rpc-server
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
