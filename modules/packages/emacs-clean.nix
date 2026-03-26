{ self, inputs, ... }:
let
  selfLib = self.lib.self;
in
{
  perSystem =
    { pkgs, lib, ... }:
    let
      emacs-clean-with-packages =
        basePackage:
        basePackage.pkgs.withPackages (
          e: with e; [
            consult
            corfu
            devicetree-ts-mode
            direnv
            eat
            embark
            embark-consult
            kdl-mode
            magit
            marginalia
            nix-mode
            orderless
            org-roam
            paredit
            vertico
            ws-butler
            zenburn-theme
            treesit-grammars.with-all-grammars
          ]
        );

      emacsPackage =
        {
          lib,
          symlinkJoin,
          makeWrapper,
          writeShellApplication,
          basePackage ? pkgs.emacs-git-nox,
          impureConfigDir ? "personal-workspace/nixos-config/files/emacs",
          extraPackages ? [ ],
        }:
        let
          emacsWithPackages = emacs-clean-with-packages basePackage;
          flakyConfigDir = selfLib.dir "emacs";
          emacsBinaryWrapper = writeShellApplication {
            name = "emacs-env-aware-wrapper";
            text = ''
              init_directory="${flakyConfigDir}"
              if [[ -n ''${HOME+x} && -d "$HOME/${impureConfigDir}" ]]; then
                 init_directory="$HOME/${impureConfigDir}"
              fi
              # trailing ':' makes emacs join INFOPATH and Info-default-directory-alist
              if [[ -n ''${INFOPATH+x} && ! $INFOPATH =~ :$ ]]; then
                export INFOPATH="$INFOPATH:"
              fi
              exec "${lib.getExe emacsWithPackages}" \
                --init-directory="$init_directory" \
                "$@"
            '';
          };
          b-emacsclient = writeShellApplication {
            name = "b-emacsclient";
            runtimeInputs = [
              emacsWithPackages
            ];
            text = ''
              exec emacsclient --no-wait --alternate-editor="${lib.getExe' pkgs.coreutils "true"}" "$@"
            '';
          };
        in
        symlinkJoin {
          name = "emacs-from-nixos-config";
          paths =
            with pkgs;
            [
              emacsWithPackages
              yamllint
              yamlfmt
              nixfmt
              fd
              ripgrep
              b-emacsclient
            ]
            ++ extraPackages;
          postBuild = ''
            ln -sf "${lib.getExe emacsBinaryWrapper}" "$out/bin/emacs"
          '';
          meta.mainProgram = "emacs";
        };

    in
    {
      packages.emacs = pkgs.callPackage emacsPackage { };
      packages.emacs-pgtk = pkgs.callPackage emacsPackage { basePackage = pkgs.emacs-git-pgtk; };
      packages.emacs-nox = pkgs.callPackage emacsPackage { basePackage = pkgs.emacs-git-nox; };
    };
}
