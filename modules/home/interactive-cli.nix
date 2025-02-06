{flake, lib, pkgs, config, ...}:
let
  defEnable = config.hostConfig.lib.defaults.enable;
  fzf_show_file_or_dir_preview="if [ -d {} ]; then lsd --tree --color=always {} | head -200; else bat -n --color=always --line-range :500 {}; fi";
in {

  # XXX 2025-01-31 daemon mode for atuin, maybe remove soon
  imports = [
    "${flake.inputs.home-manager-master}/modules/programs/atuin.nix"
    flake.inputs.nix-index-database.hmModules.nix-index
  ];
  disabledModules = [ "${flake.inputs.home-manager}/modules/programs/atuin.nix" ];

  options = {
    programs.doggo.enable = lib.mkEnableOption "Install `doggo` (`dig` replacement)";
    programs.ssh.stableAgentSocket = lib.mkEnableOption "Use stable path for ssh-agent socket, so it can be easily replaced inside long running processes like tmux/emacs/...";
  };

  config = lib.mkMerge [
    (lib.mkIf config.hostConfig.feature.interactive-cli {

      programs.atuin = {
        enable = defEnable;
        package = pkgs.bleeding.atuin; # at least 18.4.0 for proper socket handling on impermanence machines
        enableZshIntegration = defEnable;
        enableBashIntegration = defEnable;
        settings = {
          search_mode = "fuzzy";
        };
        daemon.enable = defEnable;
      };

      programs.bash.enable = defEnable;

      programs.bat.enable = defEnable;

      programs.broot = {
        enable = defEnable;
        enableZshIntegration = defEnable;
        enableBashIntegration = defEnable;
      };

      programs.btop.enable = defEnable;

      programs.darcs.enable = defEnable;

      programs.direnv = {
        enable = defEnable;
        enableZshIntegration = defEnable;
        nix-direnv = {
          enable = defEnable;
        };
        config.global = {
          # Make direnv messages less verbose
          hide_env_diff = true;
        };
      };

      programs.doggo.enable = defEnable;

      programs.fd = {
        enable = defEnable;
        ignores = [ ".git/" ".direnv/" ];
        hidden = true;
      };

      programs.fzf = {
        enable = defEnable;
        defaultCommand = "fd";
        fileWidgetOptions = ["--preview '${fzf_show_file_or_dir_preview}'"];
        changeDirWidgetOptions = ["--preview 'lsd --tree --color=always {} | head -200'"];
        tmux = {
          enableShellIntegration = true;
          shellIntegrationOptions = [ "-d 40%" ];
        };
      };

      programs.helix.enable = defEnable;

      programs.htop.enable = defEnable;

      programs.jq.enable = defEnable;

      programs.lsd.enable = defEnable;

      programs.nix-index = {
        enable = true;
        enableZshIntegration = true;
      };

      programs.nix-index-database.comma.enable = true;

      programs.rtorrent = {
        enable = defEnable;
        extraConfig = ''
          encoding.add=utf-8
        '';
      };

      programs.ssh = {
        enable = defEnable;
        stableAgentSocket = defEnable;
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

      services.ssh-agent.enable = true;

      programs.starship = {
        enable = defEnable;
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

      programs.tmux = {
        baseIndex = 1;
        clock24 = true;
        enable = defEnable;
        shortcut = "o";
        terminal = "screen-256color"; # needed, e.g. for emacs -nw
        # set -g window-status-current-style bg=red
        extraConfig = ''
        set -g allow-rename off
        set -g update-environment "DISPLAY KRB5CCNAME SSH_ASKPASS SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY"
        setenv -g SSH_AUTH_SOCK ${config.xdg.stateHome}/ssh/stable_ssh_auth_sock
      '';
      };

      programs.zellij = {
        enable = true;
      };

      programs.zsh = {
        enable = defEnable;
        autocd = defEnable;
        autosuggestion.enable = defEnable;
        syntaxHighlighting.enable = defEnable;

        shellAliases = {
          vi = "emacsclient -a 'emacs -nw' -nw";
          vim = "emacsclient -a 'emacs -nw' -nw";
          o = ''xdg-open'';
          pst = ''pstree -apU | less'';
          tldr = ''tldr --quiet'';
        };

        dirHashes = {
          docs = config.xdg.userDirs.documents;
          dl = config.xdg.userDirs.download;
        };

        initExtra = ''
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
          export LS_COLORS="$(${lib.getExe pkgs.vivid} generate zenburn)"
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

      programs.zoxide.enable = true;

      programs.gh.enable = true;

      home.packages = with pkgs; [
        age
        ansible
        binutils # 'strings' mostly
        btrfs-progs
        curl
        curlie
        deploy-rs
        docker-compose
        docker-credential-helpers
        doggo
        duf
        e2fsprogs
        elinks
        exfatprogs
        file
        gdb
        git-annex
        git-crypt
        gnum4
        gnumake
        gnupg
        gopass
        gron
        httpie
        inetutils
        ipcalc
        jless
        just
        lsof
        man-pages
        mc
        nil # nix language server
        nix-output-monitor
        nmap
        ntfsprogs
        openssl
        ov
        p7zip
        parallel
        parted
        psmisc
        pv
        python3
        recode
        reptyr
        ripgrep
        socat
        sops
        sshfs
        sysstat
        tcpdump
        trezor-agent
        unrar
        unzip
        wget
        which
        whois
        wol
        yubikey-manager
        zip
      ];
    })
    (lib.mkIf config.programs.atuin.enable {
      home.sessionVariables.ATUIN_NOBIND = "1"; # XXX why I did it?
    })
    (lib.mkIf config.programs.fzf.enable {
      programs.doggo.enable = true;
      programs.fd.enable = true;
      programs.lsd.enable = true;
      programs.zsh.initExtra = ''
        _fzf_comprun() {
          local command=$1
          shift

          case "$command" in
            cd)           fzf --preview 'lsd --tree --color=always {} | head -200' "$@" ;;
            export|unset) fzf --preview "eval 'echo ''${}'"         "$@" ;;
            ssh)          fzf --preview 'doggo {}'                  "$@" ;;
            *)            fzf --preview "${fzf_show_file_or_dir_preview}" "$@" ;;
          esac
        }
      '';
    })
    (lib.mkIf (config.programs.ssh.enable && config.programs.ssh.stableAgentSocket) (
      lib.mkMerge [
        {
          home.file.".ssh/rc".text = ''
            # Fix SSH auth socket location so agent forwarding works with tmux
            if test "$SSH_AUTH_SOCK" ; then
                ln -sf $SSH_AUTH_SOCK ${config.xdg.stateHome}/ssh/stable_ssh_auth_sock
            fi
          '';
          home.activation.createSshStateDirs = lib.hm.dag.entryAfter ["linkGeneration"] ''
            mkdir -p ${config.xdg.stateHome}/ssh/
          '';
        }
        (lib.mkIf config.hostConfig.feature.gui {
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
        })
      ]))
    (lib.mkIf config.programs.starship.enable {
      home.sessionVariables = {
        STARSHIP_CACHE = lib.mkDefault "${config.xdg.cacheHome}/starship";
      };
    })
    (lib.mkIf config.programs.zellij.enable {
      xdg.configFile."zellij/config.kdl".source = config.lib.self.file "zellij.kdl";
      home.sessionVariables."ZELLIJ_CONFIG_FILE" = "${config.xdg.configHome}/zellij/config.kdl";
    })
    (lib.mkIf config.programs.zsh.syntaxHighlighting.enable {
      programs.zsh.initExtra = ''
        # there is an option in home-manager module, but then I can't set it dynamically
        source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        ZSH_HIGHLIGHT_STYLES+=('comment' 'fg=white,bold')
        if [[ $COLORTERM = *(24bit|truecolor)* ]]; then
          ZSH_HIGHLIGHT_STYLES[comment]="fg=#999"
        elif type echotc > /dev/null && [[ $(echotc Co) == 256 ]]; then
          ZSH_HIGHLIGHT_STYLES[comment]="fg=#245"
        fi
      '';
    })
    (lib.mkIf config.hostConfig.feature.gui {
      home.packages = with pkgs; [
        gparted
      ];
    })
  ];
}
