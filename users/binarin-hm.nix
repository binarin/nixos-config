{ lib, pkgs, config, system, ... }:

let

  libcaption = pkgs.stdenv.mkDerivation rec {
    pname = "libcaption";
    version = "e8b6261090eb3f2012427cc6b151c923f82453db";
    src = pkgs.fetchFromGitHub {
      owner = "szatmary";
      repo = "libcaption";
      rev = version;
      sha256 = "sha256-9tszEKR30GHoGQ3DE9ejU3yOdtDiZwSZHiIJUPLgOdU=";
    };

    nativeBuildInputs = with pkgs; [ cmake ];
  };

  erlang-ls-patched = pkgs.beam.packages.erlang.erlang-ls.overrideAttrs (prev: rec {
    version = "0.46.0";
    name = "erlang-ls-${version}";
    src = pkgs.fetchFromGitHub {
      owner = "erlang-ls";
      repo = "erlang_ls";
      sha256 = "sha256-4h+wD/JwUulejezyHnZFrR8GF5UmdZG1DhRjjg/CkyM=";
      rev = version;
    };
  });

  obs-gphoto = pkgs.stdenv.mkDerivation rec {
    pname = "osb-gphoto";
    version = "0.4.0";

    src = pkgs.fetchFromGitHub {
      owner = "adlerweb";
      repo = "obs-gphoto";
      rev = "bd88ff79c6be412963d94303a7f92509a69d2751";
      sha256 = "sha256-oDdLOu3qL+L3sK9GJTj3pM/AKb9nUZw0AfPoJT4h10E=";
    };

    preConfigure = ''
      substituteInPlace CMakeLists.txt \
        --replace \$\{LIBOBS_PLUGIN_DESTINATION\} $out/lib/obs-plugins
    '';

    nativeBuildInputs = with pkgs; [ cmake pkg-config ninja ];
    buildInputs = with pkgs; [ wayland obs-studio xorg.libX11 libgphoto2 libjpeg udev libcaption ];
    cmakeFlags = [
      "-DSYSTEM_INSTALL=ON"
    ];

  };

  ignoringVulns = x: x // { meta = (x.meta // { knownVulnerabilities = [ ]; }); };
  qtwebkitIgnoringVulns = pkgs.qt5.qtwebkit.overrideAttrs ignoringVulns;
in
{
  # imports = [
  #   ./emacs-hm.nix
  # ];
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

  fonts.fontconfig.enable = lib.mkForce true;

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
      docs  = "$HOME/Documents";
      dl    = "$HOME/Downloads";
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
    (goldendict.override { qtwebkit = qtwebkitIgnoringVulns; })
    (wrapOBS { plugins = with pkgs.obs-studio-plugins; [ wlroots ]; })
    docker-credential-helpers
    anki-bin
    ansible
    # aws-iam-authenticator
    # awscli2
    bazel_6
    bleeding.yt-dlp
    comma
    cuetools
    discord
    docker-compose
    electrum
    # elixir_1_14
    # entr
    # erlangR25
    esphome
    ffmpeg
    flac
    git-annex
    gnumake
    gnupg
    gopass
    gparted
    gphoto2
    # helix
    htop
    httpie
    jetbrains.idea-community
    k0sctl
    k9s
    kapp
    kdenlive
    kid3
    kind
    krew
    kubectx
    age
    deploy-rs
    kubernetes
    kubernetes-helm
    # lilypond-with-fonts
    mac
    mitmproxy
    ov
    packer
    parinfer-rust
    picard
    protonmail-bridge
    python3
    recode
    remmina
    ripgrep
    shntool
    signal-desktop
    skaffold
    sops
    sox
    sshfs
    tdesktop
    # terraform-ls
    # terraform-providers.google
    # terraform_1
    thunderbird
    vlc
    wdisplays
    winePackages.full
    # wt-maker
    ytt
    rxvt-unicode # XXX for sshmenu
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
  home.sessionVariables.EDITOR = "emacsclient -a '' -nw"; # XXX home-manager mangles '' at the end of the string for whatever reason

  programs.ssh = {
    enable = true;
  };

  programs.rtorrent = {
    enable = true;
    extraConfig = ''
      encoding.add=utf-8
    '';
  };

  nix.gc.automatic = true;
  nix.gc.options = "-d --delete-older-than 30d";
}
