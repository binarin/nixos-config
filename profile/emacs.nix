{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.programs.emacs;
  overrides = self: super: with self; rec {
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

  packages = (p: with p; [
    ace-window
    alchemist
    amx
    anaphora
    # auctex
    auto-complete # edts dep
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
    eproject # edts dep
    erlang
    evil
    eyebrowse
    f
    firestarter
    fsm
    #    geiser
    general
    go-mode
    flycheck-gometalinter
    company-go
    go-eldoc
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
    popup # edts dep
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

  emacs-with-packages = ((pkgs.bleeding.emacsPackagesFor cfg.package).overrideScope' overrides).emacsWithPackages packages;

in
{
  options = {
    programs = {
      emacs = {
        package = mkOption {
          type = types.package;
          default = pkgs.bleeding.emacsPgtkNativeComp;
          description = ''
            Emacs package to bundle with all the modes
          '';
        };
      };
    };
  };
  config = {
    environment.systemPackages =
      [
        emacs-with-packages
      ] ++ (with pkgs; [
        # xprintidle-ng
        sqlite # for helm-dash
        # gometalinter
        metals # scala LSP
      ]);
  };
}
