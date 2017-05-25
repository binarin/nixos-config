{config, pkgs, ...}:

let
  emacs = with pkgs.bleeding; emacs25.override {
    withGTK3 = true;
    withGTK2 = false;
    # withXwidgets = true;
    withCsrc = true;
    inherit (pkgs) imagemagick gtk3; # webkitgtk24x;
  };
  gen = with pkgs.bleeding; emacsPackagesNgGen emacs;
  emacs-with-packages = gen.emacsWithPackages (p: with p; [
    alchemist
    anaphora
    auctex
    auto-complete  # edts dep
    auto-highlight-symbol # edts dep
    cider
    circe
    company
    counsel
    dash
    easy-escape
    editorconfig
    elisp-slime-nav
    elm-mode
    # elpy
    eproject  # edts dep
    erlang
    evil
    f
    geiser
    go-mode flycheck-gometalinter company-go
    ghc-mod
    haskell-mode
    helm
    helm-dash
    helm-projectile
    highlight-parentheses
    htmlize
    intero
    ivy-hydra
    key-chord
    keyfreq
    magit
    markdown-mode
    mu4e-maildirs-extension
    nix-mode
    org-plus-contrib
    paredit
    pdf-tools
    popup  # edts dep
    projectile
    pt
    puppet-mode
    request
    s
    slime
    smart-mode-line
    structured-haskell-mode
    symbol-overlay
    undo-tree
    web-mode
    ws-butler
    yaml-mode
    yasnippet
    zenburn-theme
    zoom-frm
  ]);
in
{
  imports = [
    ../packages/bleeding-edge.nix
  ];
  environment.systemPackages =
    [
      emacs-with-packages
    ] ++ (with pkgs; [
      xprintidle-ng
      sqlite # for helm-dash
    ]);
}
