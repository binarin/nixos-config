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
    f
    helm
    helm-dash
    helm-projectile
    highlight-parentheses
    htmlize
    (pkgs.stdenv.lib.overrideDerivation intero (oldAttrs: {
      src = pkgs.fetchFromGitHub {
        owner = "commercialhaskell";
        repo = "intero";
        rev = "2f0e0ef576dc9bf65bc8bec6a43f6f133faaa64f";
        sha256 = "1m26pp4ii38ml86ca9jpi1cm0r561k07z639ls72m1k3pzs22yp0";
      };
    }))
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
