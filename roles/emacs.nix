{config, pkgs, ...}:

let
  emacs = with config.bleeding.pkgs; emacs25pre.override {
    withGTK3 = true;
    withGTK2 = false;
    withXwidgets = true;
    inherit (pkgs) imagemagick webkitgtk24x gtk3;
  };
  gen = with config.bleeding.pkgs; emacsPackagesNgGen emacs;
  emacs-with-packages = gen.emacsWithPackages (p: with p; [
    auctex
    emacs-source-directory
    anaphora
    auto-complete eproject popup auto-highlight-symbol # edts deps
    dash
    elisp-slime-nav
    erlang
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
    elpy
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
