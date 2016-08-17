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
    # htmlize upstream repo is lost forever
    (pkgs.stdenv.lib.overrideDerivation htmlize (oldAttrs: {
      src = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/abo-abo/htmlize/363444135b671c6f4373cdacad162f0409dda2d8/htmlize.el";
        sha256 = "1j2h3zjrykqb5ypk2mbv3c9cxmnf6jc6q4in73cqfvb30n8gnh79";
      };
    }))
    (pkgs.stdenv.lib.overrideDerivation intero (oldAttrs: {
      src = pkgs.fetchFromGitHub {
        owner = "commercialhaskell";
        repo = "intero";
        rev = "54d222d02b5ebb8d4c968e70d8721fa95d7d9071";
        sha256 = "0qy0pcz3pivl1193dggfir09gw9rc9a1w1xp3iq80hnyama07zhw";
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
    # zoom-frm
    (pkgs.stdenv.lib.overrideDerivation zoom-frm (oldAttrs: {
      src = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/emacsmirror/zoom-frm/fb92370ec6ed52c09b903ac3d1eed944c1d0621e/zoom-frm.el";
        sha256 = "1whpd97yjby5zbcr4fcn0nxhqvn6k3jn8k2d15i6ss579kziwdqn";
      };
    }))
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
