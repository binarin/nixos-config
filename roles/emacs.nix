{config, pkgs, ...}:

let
  overrides = super: self: rec {
    inherit (self.melpaPackages) indium haskell-mode elisp-refs;
    jabber = (super.jabber.overrideAttrs (oldAttrs: {
      version = "2017-04-23";
      packageRequires = [ super.fsm ];
      propagatedBuildInputs = [ super.fsm ];
      src = pkgs.fetchFromGitHub {
        owner = "legoscia";
        repo = "emacs-jabber";
        rev = "3de7fb40ab9c82ada2a4b5f364a2417345953050";
        sha256 = "0miq8y9yfnhihwxayzri81s21qwqm5vyj3h7j95q5kmdml661fb4";
      };
      org-gcal = self.melpaPackages.org-gcal;
    }));
    shm = (self.callPackage ({ fetchFromGitHub, fetchurl, lib, melpaBuild }:
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
        }) {}
    );
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
    magit
    markdown-mode
    mu4e-maildirs-extension
    nix-mode
    org-gcal
    org-plus-contrib
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
