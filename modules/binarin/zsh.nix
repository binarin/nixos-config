{ ... }:
{
  flake.homeModules.binarin-zsh =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.home.binarin-zsh";
      programs.zsh = {
        enable = true;
        autocd = true;
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;

        shellAliases = {
          vi = "emacsclient -a 'emacs -nw' -nw";
          vim = "emacsclient -a 'emacs -nw' -nw";
          o = "xdg-open";
          lst = "lsa --tree";
        };

        initContent = ''
          # Dir=/some/path
          # cd ~Dir
          setopt cdablevars

          # if type -p devenv > /dev/null; then
          #   eval "$(devenv hook zsh)"
          # fi

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

          if [ -d $HOME/.cargo/bin ] && [[ ":$PATH:" != *"::"* ]]; then
            PATH="$HOME/.cargo/bin''${PATH:+":$PATH"}"
          fi

          # let a terminal/tmux to keep track of a current directory to open new window in the same place
          function osc7-pwd() {
              emulate -L zsh # also sets localoptions for us
              setopt extendedglob
              local LC_ALL=C
              printf '\e]7;file://%s%s\e\' $HOST ''${PWD//(#m)([^@-Za-z&-;_~])/%''${(l:2::0:)$(([##16]#MATCH))}}
          }
          function chpwd-osc7-pwd() {
              (( ZSH_SUBSHELL )) || osc7-pwd
          }
          add-zsh-hook -Uz chpwd chpwd-osc7-pwd

          [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
            source "$EAT_SHELL_INTEGRATION_DIR/zsh"

          # OSC 133 (shell integration / semantic prompt) support, delimits the shell prompt from a command output
          # A - prompt start, B - input start, C - output start, D - output end, P - prompt start (alternative to A, wihtout fresh line)
          function precmd() {
              print -Pn "\e]133;A\e\\"
              if ! builtin zle; then
                  print -n "\e]133;D\e\\"
              fi
          }

          function preexec {
              print -n "\e]133;C\e\\"
          }

          export LS_COLORS="$(${lib.getExe pkgs.vivid} generate zenburn)"

          ${builtins.readFile ./atuin-zsh-widget.sh}

          # show help for built-ins
          # unalias run-help
          # autoload run-help
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
