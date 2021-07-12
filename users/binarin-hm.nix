{pkgs, config, system, ...}:

{
  home.file.".emacs".source = pkgs.runCommand "emacs-config-tangled" {} ''
    ${config.programs.emacs.finalPackage}/bin/emacs --batch --eval '(progn (package-initialize) (require (quote ob-tangle)) (org-babel-tangle-file "${./emacs-config.org}" "'$out'" "emacs-lisp") (kill-emacs))'
  '';

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
    extraPackages = epkgs: with epkgs; [
      ace-window
      alchemist
      amx
      anaphora
      # auctex
      auto-complete  # edts dep
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
      eproject  # edts dep
      erlang
      evil
      eyebrowse
      f
      firestarter
      fsm
      # geiser
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
      # lsp-haskell
      lsp-mode
      # lsp-ui
      # lsp-metals
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
      ob-elixir
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
      spinner
    ];
    overrides = self: super: with self; rec {
      # Funnily enough you can't override elpa packages via overrideAttrs', you really need this stupid elpaBuild dance
      spinner = super.spinner.override {
        elpaBuild = args: super.elpaBuild (args // {
          src = let srcLz = builtins.fetchurl {
            url = "https://elpa.gnu.org/packages/spinner-1.7.3.el.lz";
            sha256 = "188i2r7ixva78qd99ksyh3jagnijpvzzjvvx37n57x8nkp8jc4i4";
          };
          in pkgs.runCommand "spinner-unpacked" {} ''${pkgs.lzip}/bin/lzip -d -o $out ${srcLz}'';
        });
      };
    };
  };

  programs.tmux = {
    baseIndex = 1;
    clock24 = true;
    enable = true;
    shortcut = "o";
    terminal = "screen-256color";
    extraConfig = ''
      set -g window-status-current-style bg=red
      set -g allow-rename off
    '';
  };

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.direnv.enableZshIntegration = true;

  programs.fzf.enable = true;
  programs.bat.enable = true;

  programs.autojump = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
  };

  programs.broot = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
  };

  programs.zsh = {
    enable = true;
    autocd = true;
    # initExtra = ''
    #   # source ${pkgs.bleeding.zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh
    # '';
    shellAliases = {
      gl = ''git log  --pretty="%Cgreen%h %C(146)%an%Creset %s %Cred%ar"'';
      vi = ''emacsclient -nw'';
      vim = ''emacsclient -nw'';
      rgrep = ''grep -R'';
      o = ''xdg-open'';
      pst = ''pstree -ap | less'';
      zzz = ''sudo systemctl suspend'';
    };
    enableAutosuggestions = true;
    history = {
      size = 20000;
      save = 20000;
    };
    oh-my-zsh = {
      enable = true;
      plugins = [ "colored-man-pages" "dirpersist" ];
      theme = "gianu";
    };
  };

  home.stateVersion = "20.09";

  home.packages = with pkgs; [
    ripgrep
    htop
    erlang-ls
  ];

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userName = "Alexey Lebedeff";
    userEmail = "binarin@binarin.info";
  };
}
