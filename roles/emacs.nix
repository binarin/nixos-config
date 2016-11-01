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
    dash
    elisp-slime-nav
    elpy
    emacs-source-directory
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
    paredit
    pdf-tools
    popup  # edts dep
    projectile
    pt
    request
    s
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
