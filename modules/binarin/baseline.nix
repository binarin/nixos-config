{ self, ... }:
{
  flake.nixosModules.binarin-baseline =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.binarin-baseline";

      imports = [
        self.nixosModules.home-manager
        self.nixosModules.impermanence
        self.modules.generic.public-keys
      ];

      programs.zsh.enable = true;

      users.users = {
        binarin = {
          # Only makes sense for hosts used over SSH, as the
          # connection is intermittent, but everything can keep
          # functioning. But services lifetime can be frequently tied
          # to wayland session lifetime (i.e. emacs daemon), and then
          # `linger` can produce strange combinations of running
          # services - so for graphical desktop the graphical user
          # session is a more natural.
          linger = !config.services.graphical-desktop.enable;

          description = "Alexey Lebedeff";
          uid = 1000;
          isNormalUser = true;
          home = "/home/binarin";
          createHome = true;
          group = "binarin";
          shell = "/run/current-system/sw/bin/zsh";
          extraGroups = [
            "users"
            "wheel"
            "pcap"
          ];
          openssh = {
            authorizedKeys.keys = config.lib.publicKeys.ssh.secureForUser "binarin";
            authorizedPrincipals = [
              "root"
              "binarin"
            ];
          };
        };
      };

      users.groups = {
        binarin = {
          gid = 1000;
        };
      };

      nix.settings.trusted-users = [ "binarin" ];

      home-manager.users.binarin =
        { osConfig, lib, ... }:
        {
          imports = [
            self.homeModules.binarin-baseline
          ];
          config = {
            home.homeDirectory = "/home/binarin";
            home.username = "binarin";
            home.stateVersion = lib.mkDefault osConfig.system.stateVersion;
          };
        };

    };

  flake.homeModules.binarin-baseline =
    {
      lib,
      pkgs,
      osConfig,
      ...
    }:
    {
      key = "nixos-config.modules.home.binarin-baseline";

      imports = [
        self.homeModules.impermanence
      ];

      programs.fzf = {
        enable = true;
        tmux = {
          enableShellIntegration = true;
          shellIntegrationOptions = [ "-d 40%" ];
        };
      };

      programs.tmux = {
        baseIndex = 1;
        clock24 = true;
        enable = true;
        shortcut = "o";
        terminal = "screen-256color"; # needed, e.g. for emacs -nw
        mouse = true;
        focusEvents = true;
        plugins = with pkgs.tmuxPlugins; [
          {
            plugin = extrakto;
          }
          {
            plugin = pain-control;
          }
          {
            plugin = sensible;
          }
        ];

        extraConfig = ''
          # even having sensible last is not enough to do these 2 binds
          bind-key C-o last-window
          bind-key o send-prefix

          set -g word-separators ' "=()[]'
          set -ag word-separators "'"  # '-a' for append, "'" should be quoted differently

          set -g allow-rename off
          set -g update-environment "DISPLAY KRB5CCNAME SSH_ASKPASS SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY"

          set-option -s set-clipboard on
          set-option -as terminal-overrides "xterm-256color:Ms=\\E]52;c%p1%.0s;%p2%s\\7" # only for mosh - https://github.com/mobile-shell/mosh/pull/1054
        '';
      };

      programs.zoxide.enable = true;
      programs.bash.enable = true;
      programs.bat.enable = true;

      programs.broot = {
        enable = true;
        enableZshIntegration = true;
        enableBashIntegration = true;
      };

      programs.fd = {
        enable = true;
        ignores = [
          ".git/"
          ".direnv/"
        ];
        hidden = true;
      };

      programs.atuin = {
        enable = true;
        enableZshIntegration = true;
        enableBashIntegration = true;
        settings = {
          search_mode = "fuzzy";
          sync_address = "https://atuin.binarin.info";
        };
        daemon.enable = true;
      };

      impermanence.persist-directories = [
        ".local/share/atuin"
      ];

      home.sessionVariables.EDITOR = "emacsclient -a 'emacs -nw' -nw";

      programs.emacs = {
        enable = true;
        extraPackages =
          epkgs: with epkgs; [
            magit
            zenburn-theme
          ];
      };

      services.emacs = {
        # - In graphical session, emacs daemon lifetime is tied to wayland server anyway
        # - I suspect that non-daemon graphical emacs works better, i.e. with `xdg-activation-v1`
        enable = !osConfig.services.graphical-desktop.enable;
        socketActivation.enable = !osConfig.services.graphical-desktop.enable;
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

      programs.zsh = {
        enable = true;
        autocd = true;
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;

        shellAliases = {
          vi = "emacsclient -a 'emacs -nw' -nw";
          vim = "emacsclient -a 'emacs -nw' -nw";
          o = "xdg-open";
        };

        initContent = ''
          # Dir=/some/path
          # cd ~Dir
          setopt cdablevars

          rr() {
            readlink -f $(type -p $1 | awk '{print $3}')
          }

          rrb() {
            local f="$(rr $1)"
            if [[ -r "$f" ]]; then
              bat "$f"
            fi
          }

          rre() {
            local f="$(rr $1)"
            if [[ -r "$f" ]]; then
              $EDITOR "$f"
            fi
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

          [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
            source "$EAT_SHELL_INTEGRATION_DIR/zsh"

          # OSC 133 (shell integration / semantic prompt) support, delimits the shell prompt from a command output
          precmd() {
              print -Pn "\e]133;A\e\\"
          }
          export LS_COLORS="$(${lib.getExe pkgs.vivid} generate zenburn)"

          atuin-setup() {
              if ! which atuin &> /dev/null; then return 1; fi
              # bindkey '^E' _atuin_search_widget

              export ATUIN_NOBIND="true"
              eval "$(atuin init zsh)"
              fzf-atuin-history-widget() {
                  local selected num
                  setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2>/dev/null

                  # local atuin_opts="--cmd-only --limit ''${ATUIN_LIMIT:-5000}"
                  local atuin_opts="--cmd-only"
                  local fzf_opts=(
                      --height=''${FZF_TMUX_HEIGHT:-60%}
                      --tac
                      "-n2..,.."
                      --tiebreak=index
                      "--query=''${LBUFFER}"
                      "+m"
                      "--bind=ctrl-d:reload(atuin search $atuin_opts -c $PWD),ctrl-r:reload(atuin search $atuin_opts)"
                  )

                  selected=$(
                      eval "atuin search ''${atuin_opts}" |
                          fzf "''${fzf_opts[@]}"
                  )
                  local ret=$?
                  if [ -n "$selected" ]; then
                      # the += lets it insert at current pos instead of replacing
                      LBUFFER+="''${selected}"
                  fi
                  zle reset-prompt
                  return $ret
              }
              zle -N fzf-atuin-history-widget
              bindkey '^R' fzf-atuin-history-widget
          }
          atuin-setup

          # show help for built-ins
          unalias run-help
          autoload run-help
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
    };
}
