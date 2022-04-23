{lib, pkgs, config, system, ...}:

{
  home.file.".emacs".source = pkgs.runCommand "emacs-config-tangled" {} ''
    ${config.programs.emacs.finalPackage}/bin/emacs --batch --eval '(progn (package-initialize) (require (quote ob-tangle)) (org-babel-tangle-file "${./emacs-config.org}" "'$out'" "emacs-lisp") (kill-emacs))'
  '';

  home.file.".config/taffybar/taffybar.css".source = ./taffybar.css;
  home.file."bin/sshmenu".source = ./sshmenu;
  home.file.".local/share/applications/org-protocol.desktop".source = ./org-protocol.desktop;
  home.file.".local/share/applications/smart-browser-chooser.desktop".text = ''
    [Desktop Entry]
    Name=smart-browser-chooser
    Exec=sh -c "exec ${./open-link.sh} %u"
    Type=Application
    Terminal=false
    Categories=System;
    MimeType=x-scheme-handler/viber;x-scheme-handler/http;x-scheme-handler/https;x-scheme-handler/ftp;x-scheme-handler/chrome;text/html;application/x-extension-htm;application/x-extension-html;application/x-extension-shtml;application/xhtml+xml;application/x-extension-xhtml;application/x-extension-xht
  '';

  programs.info.enable = true;
  programs.emacs = {
    enable = true;
    package = if pkgs.system == "x86_64-linux" then pkgs.bleeding.emacsPgtkGcc else pkgs.emacsGcc;
    extraPackages = epkgs: with epkgs; [
      anki-editor
      bazel
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
      markdown-toc
      mu4e-maildirs-extension
      # multi-libvterm
      nix-mode
      origami
      org-roam
      org-brain
      org-gcal
      org-contrib
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

  programs.zoxide.enable = true;

  programs.fzf = {
    enable = true;
    tmux.enableShellIntegration = true;
  };
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
    initExtra = ''
      rr() {
        readlink -f $(which $1)
      }
    '';
    shellAliases = {
      gl = ''git log  --pretty="%Cgreen%h %C(146)%an%Creset %s %Cred%ar"'';
      vi = ''emacsclient -nw -a vim'';
      vim = ''emacsclient -nw -a vim'';
      rgrep = ''grep -R'';
      o = ''xdg-open'';
      pst = ''pstree -ap | less'';
      zzz = ''sudo systemctl suspend'';
      sshi = ''ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null'';
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
      custom = "$HOME/.local/share/oh-my-zsh/custom";
    };
  };

  home.file.".local/share/oh-my-zsh/custom" = {
    source = ./oh-my-zsh-custom;
    recursive = true;
  };

  home.file.".local/share/images" = {
    source = ./images;
    recursive = true;
  };

  home.stateVersion = "20.09";

  home.packages = with pkgs; [
    bleeding.anki-bin
    bazel
    comma
    elixir
    erlang
    (bleeding.erlang-ls.overrideAttrs (oldAttrs: rec {
      patches = [ ../packages/erlang-ls.diff ];
    }))
    git-annex
    gnupg
    gopass
    htop
    httpie
    ripgrep
    ytt
    kubernetes
    kubernetes-helm
    kubectx
    kapp
    krew
    bleeding.k0sctl
    bleeding.k9s
    bleeding.kind
    python3
    sox
    bleeding.terraform_1
    bleeding.terraform-ls
    bleeding.terraform-providers.google
    packer
    gnumake
    sshfs
    docker-compose
    bleeding.tdesktop
    bleeding.yt-dlp
    bleeding.yandex-disk
    wineFull
    bleeding.lilypond-with-fonts
    bleeding.vlc
  ];

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userName = "Alexey Lebedeff";
    userEmail = "binarin@binarin.info";
    delta.enable = true;
    extraConfig =  {
      core = {
        autocrlf = false;
      };
      url = {
        "git@github.com:binarin/" = { insteadOf = "gh:"; pushInsteadOf = "gh:"; };
      };
      commit = {
	      template = "${./git-commit-template.txt}";
      };
      "delta \"decorations\"" = {
        commit-decoration-style = "bold yellow box ul";
        file-style = "bold yellow ul";
        file-decoration-style = "none";
      };
      init = {
        defaultBranch = "master";
      };
    };
  };

  home.file."bin/pass" = {
    text = ''
      #!${pkgs.bash}/bin/bash
      if [[ $1 == "ls" && $# == 1 ]]; then
          exec ${pkgs.gopass}/bin/gopass ls -f
      elif [[ $1 == "rm" && $2 == "-rf" ]]; then
          shift 2
          exec ${pkgs.gopass}/bin/gopass rm -r -f "$@"
      elif [[ $1 == "show" && $# == 2 ]]; then
          shift
          exec ${pkgs.gopass}/bin/gopass show -o "$@"
      else
        exec ${pkgs.gopass}/bin/gopass "$@"
      fi
    '';
    executable = true;
  };

  home.keyboard = null;
  home.sessionVariables.EDITOR = "emacsclient -nw -a";
}
