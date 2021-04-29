{config, pkgs, ...}:

let
  overrides = self: super: with self; rec {
    smart-mode-line = super.smart-mode-line.overrideAttrs (
      oldAttrs: {
        patches = [
          (pkgs.fetchurl {
            url = "https://github.com/Malabarba/smart-mode-line/commit/6596d4d062efc9fbdeb2241cf075b47bc21f340b.diff";
            sha256 = "sha256-CJKHvrl3DeBs/CHsyWdnycVvOlqPAwAZ5Rd9IE2VwtE=";
          })
        ];
      }
    );
    # multi-libvterm = self.b {
    #   pname = "multi-libvterm";
    #   buildInputs = [ self.emacs-libvterm ];
    #   src = pkgs.fetchFromGitHub {
    #     owner = "suonlight";
    #     repo = "multi-libvterm";
    #     rev = "aaeaccf9a595fc6eb162eb229aa7e6532f4d743d";
    #     sha256 = "04aiyzgpnzk6lbvjil017d5p1zxx37x7ksz8h3m43qnnballldvj";
    #   };
    # };
  };

  customEmacsPackages = pkgs.emacsPackagesNg.overrideScope' overrides;
  gitEmacsPackages    = pkgs.emacsPackagesGen pkgs.emacsGit;


  packages = (p: with p; [
    ace-window
    alchemist
    amx
    anaphora
    # auctex
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
    eglot
    elisp-slime-nav
    direnv
    elm-mode
    elpy
    poetry
    vterm
    emojify
    eproject  # edts dep
    erlang
    evil
    eyebrowse
    f
    firestarter
    fsm
    #    geiser
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
    lsp-metals
    magit
    markdown-mode
    mu4e-maildirs-extension
    # multi-libvterm
    nix-mode
    origami
    org-roam
    org-brain
    org-gcal
    org-plus-contrib
    org-super-agenda
    ox-hugo
    paredit
    pdf-tools
    perspeen
    popup  # edts dep
    projectile
    projectile-ripgrep
    pt
    puppet-mode
    recursive-narrow
    request
    restclient
    ripgrep
    s
    sbt-mode
    scad-mode
    scala-mode
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
    wgrep
  ]);

  emacs-with-packages = customEmacsPackages.emacsWithPackages packages;
  emacs-git-with-packages = gitEmacsPackages.emacsWithPackages packages;

  emacs-pgtk-gcc = ((pkgs.emacsPackagesFor pkgs.emacsPgtkGcc).overrideScope' overrides).emacsWithPackages packages;
in
{
  imports = [
  ];
  environment.systemPackages =
    [
      # emacs-with-packages
      emacs-pgtk-gcc
      # (pkgs.buildEnv {
      #   name = "emacs-git-env";
      #   paths = [emacs-git-with-packages];
      # })
    ] ++ (with pkgs; [
      # xprintidle-ng
      sqlite # for helm-dash
      # gometalinter
      bleeding.metals # scala LSP
    ]);
}
