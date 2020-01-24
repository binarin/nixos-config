{config, pkgs, ...}:

let
  overrides = self: self: rec {
  };
  customEmacsPackages = pkgs.emacsPackagesNg.overrideScope' overrides;
  emacs-with-packages = customEmacsPackages.emacsWithPackages (p: with p; [
    ace-window
    alchemist
    amx
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
    eyebrowse
    f
    firestarter
    fsm
    geiser
    general
    go-mode flycheck-gometalinter company-go go-eldoc
    haskell-mode
    helm
    helm-dash
    helm-projectile
    helpful
    highlight-parentheses
    htmlize
    hydra
    hyperbole
    impatient-mode
    indium
    ivy-hydra
    ivy-rich
    jabber
    key-chord
    keyfreq
    key-seq
    kill-or-bury-alive
    less-css-mode
    lsp-haskell
    lsp-mode
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
    projectile-ripgrep
    pt
    puppet-mode
    recursive-narrow
    request
    ripgrep
    s
    shm
    skewer-mode
    slime
    smart-mode-line
    smart-mode-line-powerline-theme
    solarized-theme
    symbol-overlay
    tide
    # typescript-mode
    undo-tree
    vue-mode
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
      gometalinter
    ]);
}
