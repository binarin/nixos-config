{ lib, pkgs, config, system, ... }:
let
  ignoringVulns = x: x // { meta = (x.meta // { knownVulnerabilities = [ ]; }); };
  qtwebkitIgnoringVulns = pkgs.qt5.qtwebkit.overrideAttrs ignoringVulns;
in
{
  home.file."bin/sshmenu".source = ./sshmenu;
  home.file.".local/share/applications/org-protocol.desktop".source = ./org-protocol.desktop;
  home.file.".local/share/applications/smart-browser-chooser.desktop".text = ''
    [Desktop Entry]
    Name=smart-browser-chooser
    Exec=${./open-link.sh} %u
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

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.direnv.enableZshIntegration = true;

  programs.zoxide.enable = true;

  programs.bat.enable = true;

  programs.broot = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
  };

  home.sessionVariables.ATUIN_NOBIND = "1";
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
    settings = {
      search_mode = "fuzzy";
    };
  };

  programs.zsh = {
    enable = true;
    autocd = true;
    autosuggestion.enable = true;

    shellAliases = {
      vi = "emacsclient -a 'emacs -nw' -nw";
      vim = "emacsclient -a 'emacs -nw' -nw";
      emacs = "emacsclient -a 'emacs -nw' -nw";
      o = ''xdg-open'';
      pst = ''pstree -apU | less'';
      zzz = ''sudo systemctl suspend'';
    };

    dirHashes = {
      docs = "$HOME/Documents";
      dl = "$HOME/Downloads";
    };

    initExtra = ''
      # there is an option in home-manager module, but then I can't set it dynamically
      source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
      ZSH_HIGHLIGHT_STYLES+=('comment' 'fg=white,bold')
      if [[ $COLORTERM = *(24bit|truecolor)* ]]; then
        ZSH_HIGHLIGHT_STYLES[comment]="fg=#999"
      elif type echotc > /dev/null && [[ $(echotc Co) == 256 ]]; then
        ZSH_HIGHLIGHT_STYLES[comment]="fg=#245"
      fi

      # Dir=/some/path
      # cd ~Dir
      setopt cdablevars

      rr() {
        readlink -f $(which $1)
      }

      # let a terminal/tmux to keep track of a current directory to open new window in the same place
      function osc7 {
          local LC_ALL=C
          export LC_ALL

          setopt localoptions extendedglob
          input=( ''${(s::)PWD} )
          uri=''${(j::)input/(#b)([^A-Za-z0-9_.\!~*\'\(\)-\/])/%''${(l:2::0:)$(([##16]#match))}}
          print -n "\e]7;file://''${HOSTNAME}''${uri}\e\\"
      }
      add-zsh-hook -Uz chpwd osc7

      # OSC 133 (shell integration / semantic prompt) support, delimits the shell prompt from a command output
      precmd() {
          print -Pn "\e]133;A\e\\"
      }
    '';

    history = {
      size = 20000;
      save = 20000;
      share = true;
      ignoreSpace = true;
    };

    oh-my-zsh = {
      enable = true;
      plugins = [
        "aliases"
        "ansible"
        "colored-man-pages"
        "copyfile"
        "copypath"
        "dirpersist"
        "extract"
        "git"
        "systemd"
      ];
    };
  };

  home.file.".local/share/images" = {
    source = ./images;
    recursive = true;
  };

  home.packages = with pkgs; [
    # (wrapOBS { plugins = with pkgs.obs-studio-plugins; [ wlroots ]; })
    # aws-iam-authenticator
    # awscli2
    # elixir_1_14
    # entr
    # erlangR25
    # helix
    # lilypond-with-fonts
    # terraform-ls
    # terraform-providers.google
    # terraform_1
    # wt-maker
    age
    ansible
    bazel_6
    bleeding.yt-dlp
    comma
    cuetools
    deploy-rs
    docker-compose
    docker-credential-helpers
    esphome
    ffmpeg
    flac
    git-annex
    gnumake
    gnupg
    gopass
    gparted
    htop
    httpie
    k0sctl
    k9s
    kapp
    kid3
    kind
    krew
    kubectx
    kubernetes
    kubernetes-helm
    mac
    mitmproxy
    ov
    packer
    parinfer-rust
    protonmail-bridge
    python3
    recode
    ripgrep
    rxvt-unicode # XXX for sshmenu
    shntool
    skaffold
    sops
    sox
    sshfs
    ytt
  ];

  programs.gh.enable = true;

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
  home.sessionVariables.EDITOR = "emacsclient -a 'emacs -nw' -nw";

  programs.ssh = {
    enable = true;
  };

  programs.rtorrent = {
    enable = true;
    extraConfig = ''
      encoding.add=utf-8
    '';
  };
}
