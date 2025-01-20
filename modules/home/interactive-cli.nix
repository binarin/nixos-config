{
  lib,
  pkgs,
  config,
  system,
  ...
}:
let
  fzf_show_file_or_dir_preview="if [ -d {} ]; then lsd --tree --color=always {} | head -200; else bat -n --color=always --line-range :500 {}; fi";
in {
  config = lib.mkIf config.hostConfig.feature.interactive-cli {
    home.packages = with pkgs; [
      age
      ansible
      autoconf
      automake
      comma
      curlie
      deploy-rs
      docker-compose
      docker-credential-helpers
      doggo
      duf
      bleeding.forgejo-cli
      gcc
      git-annex
      gnumake
      gnupg
      gopass
      htop
      httpie
      hugo
      jless
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
      terminal = "screen-256color"; # needed, e.g. for emacs -nw
      # set -g window-status-current-style bg=red
      extraConfig = ''
        set -g allow-rename off
        set -g update-environment "DISPLAY KRB5CCNAME SSH_ASKPASS SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY"
        setenv -g SSH_AUTH_SOCK ${config.xdg.stateHome}/ssh/stable_ssh_auth_sock
      '';
    };

    programs.fd = {
      enable = true;
      ignores = [ ".git/" ".direnv/" ];
      hidden = true;
    };

    programs.direnv.enable = true;
    programs.direnv.nix-direnv.enable = true;
    programs.direnv.enableZshIntegration = true;

    programs.bash.enable = true;
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
      syntaxHighlighting.enable = true;

      shellAliases = {
        vi = "emacsclient -a 'emacs -nw' -nw";
        vim = "emacsclient -a 'emacs -nw' -nw";
        o = ''xdg-open'';
        pst = ''pstree -apU | less'';
        tldr = ''tldr --quiet'';
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

        ds() {
          nix derivation show "$@" | jless .
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

        _fzf_comprun() {
          local command=$1
          shift

          case "$command" in
            cd)           fzf --preview 'lsd --tree --color=always {} | head -200' "$@" ;;
            export|unset) fzf --preview "eval 'echo ''${}'"         "$@" ;;
            ssh)          fzf --preview 'dig {}'                   "$@" ;;
            *)            fzf --preview "${fzf_show_file_or_dir_preview}" "$@" ;;
          esac
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
      matchBlocks.mail = {
        match = ''
          host mail.lynx-lizard.ts.net,mail,${config.inventory.ipAllocation.mail.home.primary.address}
        '';
        extraOptions = {
          ControlMaster = "auto";
          HostKeyAlias = "mail.lynx-lizard.ts.net";
        };
      };
      controlPath = "~/.ssh/master-%r@%k:%p";
      userKnownHostsFile = "${config.xdg.stateHome}/ssh/known_hosts";
    };

    home.file.".ssh/rc".text = ''
      # Fix SSH auth socket location so agent forwarding works with tmux
      if test "$SSH_AUTH_SOCK" ; then
          ln -sf $SSH_AUTH_SOCK ${config.xdg.stateHome}/ssh/stable_ssh_auth_sock
      fi
    '';

    home.activation.createSshStateDirs = lib.hm.dag.entryAfter ["linkGeneration"] ''
      mkdir -p ${config.xdg.stateHome}/ssh/
    '';

    systemd.user.services.stable-ssh-agent-socket-use-local = {
      Unit = {
        After = [ "graphical-session.target" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = ''
          ${lib.getExe' pkgs.coreutils "ln"} -sf "%t/ssh-agent" ${config.xdg.stateHome}/ssh/stable_ssh_auth_sock
        '';
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    programs.rtorrent = {
      enable = true;
      extraConfig = ''
        encoding.add=utf-8
      '';
    };

    programs.fzf = {
      enable = true;
      defaultCommand = "fd";
      fileWidgetOptions = ["--preview '${fzf_show_file_or_dir_preview}'"];
      changeDirWidgetOptions = ["--preview 'lsd --tree --color=always {} | head -200'"];
      tmux = {
        enableShellIntegration = true;
        shellIntegrationOptions = [ "-d 40%" ];
      };
    };

    # Programs natively supported by home-manager.
    # They can be configured in `programs.*` instead of using home.packages.
    programs = {
      # Type `<ctrl> + r` to fuzzy search your shell history
      jq.enable = true;
      # Install btop https://github.com/aristocratos/btop
      btop.enable = true;
    };

    # home.file."${config.xdg.cacheHome}/tealdeer/tldr-pages" = {
    #   source = pkgs.fetchzip {
    #     url = "https://github.com/tldr-pages/tldr/releases/download/v2.2/tldr.zip";
    #     hash = "sha256-QM5nMRO74LeyG6VB9rFND8Ez6lzG8A512YudoxvlugI=";
    #     stripRoot = false;
    #   };
    # };

    programs.tealdeer = {
      enable = true;
      settings = {

        updates = {
          auto_update = false;
        };
      };
    };

    programs.starship = {
      enable = true;
      settings = {
        username = {
          style_user = "blue bold";
          style_root = "red bold";
          format = "[$user]($style) ";
          disabled = false;
          show_always = true;
        };
        hostname = {
          ssh_only = false;
          ssh_symbol = "🌐 ";
          format = "on [$hostname](bold red) ";
          trim_at = ".local";
          disabled = false;
        };
        shlvl = {
          disabled = false;
          symbol = "↕️";
          repeat = true;
          repeat_offset = 3;
          format = "[$symbol](bold yellow) ";
        };
      };
    };

    programs.zellij = {
      enable = true;
    };

    xdg.configFile."zellij/config.kdl".source = config.lib.self.file "zellij.kdl";
    home.sessionVariables."ZELLIJ_CONFIG_FILE" = "${config.xdg.configHome}/zellij/config.kdl";
  };


}
