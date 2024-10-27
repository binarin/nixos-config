{ config, pkgs, ... }:
{
  programs.info.enable = true;
  home.file.".emacs".source = pkgs.runCommand "emacs-config-tangled" { } ''
    ${config.programs.emacs.finalPackage}/bin/emacs --batch --eval '(progn (package-initialize) (require (quote ob-tangle)) (org-babel-tangle-file "${./emacs-config.org}" "'$out'" "emacs-lisp") (kill-emacs))'
  '';


  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs: with epkgs; [
      sudo-edit
      corfu
      consult
      marginalia
      orderless
      vertico
      embark
      embark-consult
      unicode-fonts
      anki-editor
      bazel
      ace-window
      alchemist
      amx
      anaphora
      # auctex
      auto-complete # edts dep
      auto-highlight-symbol # edts dep
      avy
      # cider
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
      # geiser
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
      # jabber
      key-chord
      keyfreq
      key-seq
      kill-or-bury-alive
      less-css-mode
      # lsp-haskell
      lsp-mode
      # lsp-ui
      # lsp-metals
      magit
      markdown-mode
      markdown-toc
      # mu4e-maildirs-extension
      # multi-libvterm
      nix-mode
      origami
      org-roam
      org-brain
      org-gcal
      org-contrib
      org-contacts
      org-super-agenda
      ob-elixir
      ox-hugo
      paredit
      parinfer-rust-mode
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
      skewer-mode
      slime
      smart-mode-line
      smart-mode-line-powerline-theme
      solarized-theme
      symbol-overlay
      terraform-mode
      company-terraform
      terraform-doc
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
      spinner
    ];
    overrides = self: super: with self; rec {
      org = self.elpaPackages.org;
      # Funnily enough you can't override elpa packages via overrideAttrs', you really need this stupid elpaBuild dance
      # spinner = super.spinner.override {
      #   elpaBuild = args: super.elpaBuild (args // {
      #     src = let srcLz = builtins.fetchurl {
      #       url = "https://elpa.gnu.org/packages/spinner-1.7.3.el.lz";
      #       sha256 = "188i2r7ixva78qd99ksyh3jagnijpvzzjvvx37n57x8nkp8jc4i4";
      #     };
      #     in pkgs.runCommand "spinner-unpacked" {} ''${pkgs.lzip}/bin/lzip -d -o $out ${srcLz}'';
      #   });
      # };
    };
  };
}
