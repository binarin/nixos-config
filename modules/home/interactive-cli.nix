{
  lib,
  pkgs,
  config,
  system,
  ...
}:
{
  config = lib.mkIf config.hostConfig.feature.interactive-cli {
    home.packages = with pkgs; [
      age
      ansible
      autoconf
      automake
      comma
      deploy-rs
      docker-compose
      docker-credential-helpers
      gcc
      git-annex
      gnumake
      gnupg
      gopass
      htop
      httpie
      hugo
      lsyncd
      mac
      ov
      pkg-config
      python3
      recode
      ripgrep
      sops
      sshfs
      trezor-agent
    ];

    programs.dircolors = {
      enable = true;
      settings = {
        DIR = "01;34;46";
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
          readlink -f $(type -p $1 | awk '{print $3}')
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
    programs.gh.enable = true;
    programs.ssh = {
      enable = true;
    };
    programs.rtorrent = {
      enable = true;
      extraConfig = ''
        encoding.add=utf-8
      '';
    };
  };
}
