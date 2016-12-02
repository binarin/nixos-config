{config, pkgs, ...}:

let
  emacs = with config.bleeding.pkgs; emacs25.override {
    withGTK3 = true;
    withGTK2 = false;
    withXwidgets = true;
    withCsrc = true;
    inherit (pkgs) imagemagick webkitgtk24x gtk3;
  };
  gen = with config.bleeding.pkgs; emacsPackagesNgGen emacs;
  emacs-with-packages = gen.emacsWithPackages (p: with p; [
    alchemist
    anaphora
    auctex
    auto-complete  # edts dep
    auto-highlight-symbol # edts dep
    cider
    dash
    editorconfig
    elisp-slime-nav
    elpy
    eproject  # edts dep
    erlang
    evil
    f
    helm
    helm-dash
    helm-projectile
    highlight-parentheses
    htmlize
    intero
    key-chord
    keyfreq
    magit
    mu4e-maildirs-extension
    nix-mode
    org-plus-contrib
    paredit
    pdf-tools
    popup  # edts dep
    projectile
    pt
    request
    s
    slime
    smart-mode-line
    undo-tree
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
      sqlite # for helm-dash
    ]);
}
