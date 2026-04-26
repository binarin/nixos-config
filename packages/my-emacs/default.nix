{
  lib,
  symlinkJoin,
  buildEnv,
  yamllint,
  yamlfmt,
  nixfmt,
  fd,
  ripgrep,
  coreutils,
  writeShellApplication,
  emacsPackagesFor,

  emacsBasePackage,
  emacsPackagesFn ? (_: [ ]),
  extraPackages ? [ ],
  impureConfigDir ? "personal-workspace/nixos-config/files/emacs",
  flakyConfigDir,
  ...
}:
let
  mkEmacsWithPackages =
    basePackage:
    (emacsPackagesFor basePackage).emacsWithPackages (
      e:
      with e;
      [
        anki-editor
        avy
        bazel
        consult
        corfu
        devicetree-ts-mode
        direnv
        eat
        embark
        embark-consult
        haskell-mode
        hledger-mode
        hyperbole
        kdl-mode
        magit
        marginalia
        markdown-mode # markdown-ts-mode is already included, but this is for eglot/eldoc
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
      ++ emacsPackagesFn e
    );

  emacsWithPackages = mkEmacsWithPackages emacsBasePackage;

  emacsBinaryWrapper = writeShellApplication {
    name = "emacs-env-aware-wrapper";
    text = ''
      init_directory="${flakyConfigDir { inherit emacsWithPackages; }}"
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

buildEnv {
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
    ls -la $out/share
    ln -sf "${lib.getExe emacsBinaryWrapper}" "$out/bin/emacs"
  '';

  meta.mainProgram = "emacs";
}
