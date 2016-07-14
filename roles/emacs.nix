{config, ...}:

let
  emacs = with config.bleeding.pkgs; emacs25pre.override {
    withGTK3 = true;
    withGTK2 = false;
    gtk3 = gtk3;
  };
  gen = with config.bleeding.pkgs; emacsPackagesNgGen emacs;
  emacs-with-packages = gen.emacsWithPackages (p: with p; [
    auto-complete eproject popup auto-highlight-symbol # edts deps
    helm
    projectile
    undo-tree
    helm-projectile
    magit
    f
    s
    dash
    anaphora
    request
    yasnippet
    erlang
    nix-mode
    paredit
    ws-butler
    keyfreq
    mu4e-maildirs-extension
    zenburn-theme
    smart-mode-line
    pt
    highlight-parentheses
    zoom-frm
    elisp-slime-nav
    key-chord
    htmlize
    intero
    yaml-mode
  ]);
in
{
  imports = [
    ../packages/bleeding-edge.nix
  ];
  environment.systemPackages = [emacs-with-packages];
}
