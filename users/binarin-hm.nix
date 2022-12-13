{lib, pkgs, config, system, ...}:

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

  ignoringVulns = x: x // { meta = (x.meta // { knownVulnerabilities = []; }); };
  qtwebkitIgnoringVulns = pkgs.qt5.qtwebkit.overrideAttrs ignoringVulns;
in {
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

  home.sessionVariables.ATUIN_NOBIND = "1";
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
    settings = {
      search_mode = "fuzzy";
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

  fonts.fontconfig.enable = lib.mkForce true;

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
      bindkey '^r' _atuin_search_widget
      function osc7 {
          local LC_ALL=C
          export LC_ALL

          setopt localoptions extendedglob
          input=( ''${(s::)PWD} )
          uri=''${(j::)input/(#b)([^A-Za-z0-9_.\!~*\'\(\)-\/])/%''${(l:2::0:)$(([##16]#match))}}
          print -n "\e]7;file://''${HOSTNAME}''${uri}\e\\"
      }
      add-zsh-hook -Uz chpwd osc7

      precmd() {
          print -Pn "\e]133;A\e\\"
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

  home.packages = with pkgs; [
    (goldendict.override { qtwebkit = qtwebkitIgnoringVulns; })
    shntool
    flac
    cuetools
    kid3
    picard
    moonlight-qt
    kind
    skaffold
    jetbrains.idea-community
    krew
    helix
    gphoto2
    ffmpeg
    wdisplays
    (wrapOBS { plugins = with pkgs.obs-studio-plugins; [ wlroots ]; })
    thunderbird
    signal-desktop
    entr
    anki-bin
    bazel_6
    comma
    elixir_1_14
    erlangR25
    # nodePackages.browser-sync
    # (bleeding.erlang-ls.overrideAttrs (oldAttrs: rec {
    #   patches = [ ../packages/erlang-ls.diff ];
    # }))
    erlang-ls-patched
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
    k0sctl
    k9s
    kind
    python3
    sox
    terraform_1
    terraform-ls
    terraform-providers.google
    packer
    gnumake
    sshfs
    docker-compose
    tdesktop
    bleeding.yt-dlp
    # bleeding.yandex-disk
    winePackages.full
    lilypond-with-fonts
    vlc

    # fonts
    # corefonts
    # dejavu_fonts
    # emacs-all-the-icons-fonts
    # fira
    # fira-code
    # font-awesome
    # inconsolata
    # iosevka
    # jetbrains-mono
    # liberation_ttf
    # # mplus-outline-fonts
    # noto-fonts
    # noto-fonts-emoji
    # powerline-fonts
    # roboto
    # roboto-mono
    # roboto-slab
    # source-code-pro
    # terminus_font_ttf
    # ubuntu_font_family
    # unifont
    # vistafonts
    # terminus_font
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
