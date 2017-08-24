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
    (melpaPackages.helpful.override {elisp-refs = melpaPackages.elisp-refs; })
    highlight-parentheses
    htmlize
    intero
    ivy-hydra
    jabber
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
    (callPackage ({ fetchFromGitHub, fetchurl, lib, melpaBuild }:
    melpaBuild {
        pname = "shm";
        version = "20170523";
        src = fetchFromGitHub {
          owner = "chrisdone";
          repo = "structured-haskell-mode";
          rev = "bd08a0b2297667e2ac7896e3b480033ae5721d4d";
          sha256 = "14rl739z19ns31h9fj48sx9ppca4g4mqkc7ccpacagwwf55m259c";
        };
        recipeFile = fetchurl {
          url = "https://raw.githubusercontent.com/milkypostman/melpa/68a2fddb7e000487f022b3827a7de9808ae73e2a/recipes/shm";
          sha256 = "1qmp8cc83dcz25xbyqd4987i0d8ywvh16wq2wfs4km3ia8a2vi3c";
          name = "shm";
        };
        packageRequires = [];
        meta = {
          homepage = "https://melpa.org/#/shm";
          license = lib.licenses.free;
        };
      }) {})
    symbol-overlay
    undo-tree
    web-mode
    ws-butler
    yaml-mode
    yasnippet
    zenburn-theme
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
