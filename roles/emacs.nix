{config, pkgs, ...}:

let
  overrides = super: self: rec {
    inherit (self.melpaPackages) indium haskell-mode elisp-refs;
  };
  customEmacsPackages = pkgs.emacsPackagesNg.overrideScope overrides;
  emacs-with-packages = customEmacsPackages.emacsWithPackages (p: with p; [
    ace-window
    alchemist
    anaphora
    auctex
    auto-complete  # edts dep
    auto-highlight-symbol # edts dep
    avy
    cider
    circe
    company
    counsel
    counsel-projectile
    dante
    dash
    easy-escape
    editorconfig
    elisp-slime-nav
    elm-mode
    emojify
    eproject  # edts dep
    erlang
    evil
    f
    firestarter
    fsm
    geiser
    general
    go-mode flycheck-gometalinter company-go
    haskell-mode
    helm
    helm-dash
    helm-projectile
    helpful
    highlight-parentheses
    htmlize
    hyperbole
    impatient-mode
    indium
    intero
    ivy-hydra
    ivy-rich
    jabber
    key-chord
    keyfreq
    key-seq
    kill-or-bury-alive
    less-css-mode
    lsp-haskell
    lsp-ui
    magit
    markdown-mode
    mu4e-maildirs-extension
    nix-mode
    origami
    org-brain
    org-gcal
    org-plus-contrib
    ox-hugo
    paredit
    pdf-tools
    popup  # edts dep
    projectile
    pt
    puppet-mode
    recursive-narrow
    request
    s
    shm
    skewer-mode
    slime
    smart-mode-line
    smart-mode-line-powerline-theme
    symbol-overlay
    undo-tree
    web-mode
    which-key
    ws-butler
    yaml-mode
    yasnippet
    zenburn-theme
  ]);
in
{
  imports = [
  ];
  environment.systemPackages =
    [
      emacs-with-packages
    ] ++ (with pkgs; [
      xprintidle-ng
      sqlite # for helm-dash
    ]);
}
