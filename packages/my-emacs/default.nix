{
  lib,
  symlinkJoin,
  yamllint,
  yamlfmt,
  nixfmt,
  fd,
  ripgrep,
  coreutils,
  writeShellApplication,
  emacsPackagesFor,

  emacsBasePackage,
  extraPackages ? [ ],
  impureConfigDir ? "personal-workspace/nixos-config/files/emacs",
  flakyConfigDir,
  ...
}:
let
  mkEmacsWithPackages =
    basePackage:
    (emacsPackagesFor basePackage).emacsWithPackages (
      e: with e; [
        anki-editor
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
        org-caldav
        org-roam
        paredit
        vertico
        ws-butler
        zenburn-theme
        treesit-grammars.with-all-grammars
      ]
    );

  emacsWithPackages = mkEmacsWithPackages emacsBasePackage;

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
      exec emacsclient --no-wait --alternate-editor="${lib.getExe' coreutils "true"}" "$@"
    '';
  };

in

symlinkJoin {
  name = "emacs-from-nixos-config";
  paths = [
    yamllint
    yamlfmt
    nixfmt
    fd
    ripgrep

    emacsWithPackages
    b-emacsclient
  ]
  ++ extraPackages;

  postBuild = ''
    ln -sf "${lib.getExe emacsBinaryWrapper}" "$out/bin/emacs"
  '';

  meta.mainProgram = "emacs";
}
