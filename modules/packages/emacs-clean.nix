{ self, inputs, ... }:
{
  perSystem =
    { pkgs, lib, ... }:
    let
      emacs-clean-with-packages =
        basePackage:
        basePackage.pkgs.withPackages (
          e:
          with e;
          [
            consult
            corfu
            direnv
            magit
            marginalia
            nix-mode
            orderless
            paredit
            vertico
            zenburn-theme
            embark
            embark-consult

            treesit-grammars.with-all-grammars
          ]
          ++ (with pkgs; [
            yamllint
            yamlfmt
            nixfmt
          ])
        );

      wrap-emacs-clean =
        basePackage:
        let
          emacs-clean-package = emacs-clean-with-packages basePackage;
        in
        pkgs.runCommand "emacs-clean-wrapped"
          {
            buildInputs = with pkgs; [ makeWrapper ];
            meta.mainProgram = "emacs1";
          }
          ''
            mkdir -p $out/bin
            mkdir -p $out/share/applications
            cat ${emacs-clean-package}/share/applications/emacs.desktop | sed 's/^Exec=emacs /Exec=emacs1 /; s/^Name=Emacs/Name=Emacs(clean cfg)/'  > $out/share/applications/emacs1.desktop

            makeShellWrapper "${emacs-clean-package}/bin/emacs" "$out/bin/emacs1" \
              --inherit-argv0 \
              --add-flag -L --add-flag "/home/binarin/personal-workspace/nixos-config/files/emacs/lisp" \
              --add-flag --init-directory="/home/binarin/personal-workspace/nixos-config/files/emacs"


            makeShellWrapper "${emacs-clean-package}/bin/emacsclient" "$out/bin/emacsclient1" \
              --add-flags "--socket-name=emacs-clean" \
              --prefix PATH : "${emacs-clean-package}/bin" \
          '';

    in
    {
      packages.emacs-clean-pgtk = wrap-emacs-clean pkgs.emacs-git-pgtk;
      packages.emacs-clean-nox = wrap-emacs-clean pkgs.emacs-git-nox;
      packages.emacs-clean-batch = emacs-clean-with-packages pkgs.emacs-git-nox;
    };
}
