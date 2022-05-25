{lib, pkgs, config, system, ...}:

{
  imports = [
    ./emacs-hm.nix
  ];
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

  fonts.fontconfig.enable = true;

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
      if [ -f $HOME/.nix-profile/etc/profile.d/nix.sh ] ; then
        source /home/binarin/.nix-profile/etc/profile.d/nix.sh
      fi
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

  home.packages = with pkgs; [
    signal-desktop
    entr
    bleeding.anki-bin
    bazel
    bleeding.comma
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
    # bleeding.yandex-disk
    wineFull
    bleeding.lilypond-with-fonts
    bleeding.vlc

    # fonts
    corefonts
    dejavu_fonts
    emacs-all-the-icons-fonts
    fira
    fira-code
    font-awesome-ttf
    inconsolata
    iosevka
    jetbrains-mono
    liberation_ttf
    mplus-outline-fonts
    noto-fonts
    powerline-fonts
    roboto
    roboto-mono
    roboto-slab
    source-code-pro
    terminus_font_ttf
    ubuntu_font_family
    unifont
    vistafonts
    terminus_font
    # google-fonts
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
