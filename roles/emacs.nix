{config, pkgs, ...}:

let
  overrides = self: self: rec {
    inherit (self.melpaPackages) indium haskell-mode elisp-refs vue-mode lsp-ui lsp-mode;

    lsp-vue = self.lsp-vue.overrideAttrs (oldAttrs: { # my fix for <script> linting
      src = pkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-vue";
        rev = "5bf04a2ab07b426854e620416dcded2aff93d419";
        sha256 = "1s8bbrp2gvhjqzmw24sq58i1y3fzy93w4896rlb8ajqzjdl9j6n4";
      };
    });

    projectile = self.projectile.overrideAttrs (oldAttrs: { # improved alien indexing (`turbo-alien`)
      src = pkgs.fetchFromGitHub {
        owner = "bbatsov";
        repo = "projectile";
        rev = "f5c608d8945ff33e0289dad3a28c69878c2d2995";
        sha256 = "06srrzm4hl6khmjfvghgvgq3812fx5fxa5wrvqvcfhwcaxs7a4i5";
      };
    });

  };
  customEmacsPackages = pkgs.bleeding.emacsPackagesNg.overrideScope' overrides;
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
    go-mode flycheck-gometalinter company-go
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
    lsp-mode
    lsp-ui
    lsp-vue
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
    typescript-mode
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
    ]);
}
