{
  self,
  inputs,
  lib,
  ...
}:
let
  selfLib = self.lib.self;

  makeSystemConfig =
    (import "${self}/lib/make-system-config.nix" {
      inherit lib;
      nixos = "${inputs.nixpkgs}/nixos";
      userborn = inputs.system-manager.inputs.userborn;
      system-manager-src = inputs.system-manager;
    }).makeSystemConfig;

  bDevKvmPkgs = self.configured-pkgs.x86_64-linux.nixpkgs.appendOverlays [
    inputs.system-manager.overlays.default
  ];
in
{
  flake.systemConfigs.b-db-k = makeSystemConfig {
    pkgs = bDevKvmPkgs;
    modules = [
      self.systemModules.bentos
      self.systemModules.home-manager
      ({ lib, ... }: {
        bentos.yum.packages = [ "python3-pip" ];
      })
      ({ lib, ... }: {
        environment.etc."nix/nix.custom.conf" = {
          text = ''
            trusted-users = allebedev root 15008352
          '';
          replaceExisting = true;
        };

        users.users.allebedev = {
          name = "allebedev";
          home = "/home/allebedev";
          uid = 15008352;
          group = "users";
          isNormalUser = true;
        };

        home-manager.useGlobalPkgs = true;
        home-manager.backupFileExtension = "backup";
        home-manager.sharedModules = [ self.homeModules.home-misc ];
        home-manager.extraSpecialArgs = {
          self'.packages = self.packages.x86_64-linux;
          inputs' = lib.mapAttrs (_: i: {
            packages = i.packages.x86_64-linux;
          }) inputs;
        };
        home-manager.users.allebedev = self.homeModules.b-dev-kvm-configuration;
      })
    ];
  };

  flake.deploy.nodes.b-adb-k = {
    hostname = "adb.k.b";
    sshUser = "allebedev";
    profiles.user = {
      path = self.lib.deploy-home-manager self.homeConfigurations.b-dev-kvm;
    };
  };

  flake.deploy.nodes.b-db-k = {
    hostname = "db.k.b";
    sshUser = "allebedev";
    profiles.system = {
      user = "root";
      path = self.lib.deploy-system-manager self.systemConfigs.b-db-k;
    };
  };

  flake.homeConfigurations.b-dev-kvm = inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;

    modules = [
      self.homeModules.b-dev-kvm-configuration
    ];

    extraSpecialArgs = {
      osConfig.services.graphical-desktop.enable = false;
      osConfig.impermanence.enable = false;
      self'.packages = self.packages.x86_64-linux;
      inputs' = lib.mapAttrs (_: i: {
        packages = i.packages.x86_64-linux;
      }) inputs;
    };
  };

  flake.homeModules.standalone-home-manager-zsh =
    { lib, ... }:
    {
      key = "nixos-config.modules.home.standalone-home-manager-zsh";
      programs.zsh.envExtra = ''
        export BK_OTEL_ENABLED=false

        if [[ -f $HOME/.nix-profile/bin/zsh ]]; then
            export SHELL=$HOME/.nix-profile/bin/zsh
        fi

        # Ensure system-manager's /run/current-system/sw/bin is in PATH
        # (not guaranteed via /etc/environment on non-NixOS when PAM skips it)
        local sw_bin="/run/current-system/sw/bin"
        if [[ -d $sw_bin && ":$PATH:" != *":$sw_bin:"* ]]; then
            export PATH="$sw_bin:$PATH"
        fi

        if [[ !( $PATH == *$HOME/.local/bin* ) ]]; then
            export PATH="$HOME/.local/bin:$PATH"
        fi

        if [[ -f $HOME/.cargo/env ]] ; then
            . "$HOME/.cargo/env"
        fi

        if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
            . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
        fi

      '';
    };

  flake.homeModules.b-dev-kvm-configuration =
    {
      config,
      lib,
      pkgs,
      inputs',
      ...
    }:
    {
      key = "nixos-config.modules.home.b-dev-kvm-configuration";
      imports = [
        self.homeModules.binarin-zsh
        self.homeModules.binarin-baseline
        self.homeModules.standalone-home-manager-zsh
        self.homeModules.interactive-cli
        self.homeModules.git
      ];

      programs.ssh.enable = lib.mkForce false;

      programs.git.includes = [
        { path = selfLib.file "b-dev-kvm-gitconfig.git-crypt"; }
      ];
      programs.git.settings.credential.helper = "cache --timeout=86400";

      services.emacs = {
        enable = true;
        startWithUserSession = true;
        package = lib.mkForce config.programs.emacs.package;
      };

      services.ssh-agent.enable = lib.mkForce false;

      nixpkgs.config.allowUnfree = true;
      nixpkgs.overlays = [
        inputs.emacs-overlay.overlays.default
        self.overlays.my-emacs
        inputs.nix-ai-tools.overlays.shared-nixpkgs
      ];

      xdg.enable = false;
      programs.zsh.dotDir = config.home.homeDirectory;

      programs.zsh.envExtra = ''
        if [[ -n $SSH_AUTH_SOCK && -e $SSH_AUTH_SOCK && $SSH_AUTH_SOCK != *ssh-agent-stable.sock ]]; then
          ln -sf "$SSH_AUTH_SOCK" "$XDG_RUNTIME_DIR/ssh-agent-stable.sock"
        fi

        export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent-stable.sock"
        export BK_DISABLE_EVENTS=true

        # all of this only makes sense when I don't manage my configs with home-manager
        local -a slow_scripts=(
           /etc/profile.d/dotfiles.sh
           /etc/profile.d/bk_completion.sh
           /etc/profile.d/nvm.sh
           /etc/profile.d/ssh_warn.sh
           /etc/profile.d/completion.sh
           /etc/profile.d/puppet_warn.sh
         )

         for f in $slow_scripts; do
           if [[ -s $f ]]; then
             sudo tee $slow_scripts < /dev/null > /dev/null
             break
           fi
         done
      '';

      programs.zsh.initContent = lib.mkMerge [
        (lib.mkOrder 500 ''
          if [[ $TERM == "dumb" ]]; then
            unsetopt zle
            PS1='$ '
            return
          fi
        '')
        ''
          if type -p bk > /dev/null ; then
            source <(bk completion zsh)
          fi
        ''
      ];

      home.activation.we-own-the-configs = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
        $DRY_RUN_CMD rm -f ~/.zshrc ~/.zshenv ~/.bash_profile ~/.bashrc ~/.profile
      '';

      home.activation.fix-permissions = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p ~/.cache/oh-my-zsh/completions
        $DRY_RUN_CMD chmod 0755 ~/.cache/oh-my-zsh/completions
      '';

      programs.starship.settings.command_timeout = 2000;

      home.stateVersion = "25.11";
      home.username = "allebedev";
      home.homeDirectory = "/home/allebedev";

      home.packages =
        (with pkgs.llm-agents; [
          claude-code
          workmux
        ])
        ++ (with pkgs; [
          asciinema
          delta
          tramp-rpc-server
          (writeShellApplication {
            name = "bentos-setup";
            runtimeInputs = [ git jq ];
            text = selfLib.read "bin/bentos-setup.git-crypt";
          })
          (writeShellApplication {
            name = "bentos-patchcritic";
            runtimeInputs = [ git perl ];
            text = ''
              set -euo pipefail

              PATCHCRITIC_DIR="/usr/local/git_tree/patchcritic"

              usage() {
                echo "Usage: bentos-patchcritic [REV-RANGE|N]"
                echo ""
                echo "Run patchcritic on a git revision range."
                echo ""
                echo "Arguments:"
                echo "  REV-RANGE    A git revision range (e.g. HEAD~3..HEAD)"
                echo "  N            Number of commits back from HEAD (converted to HEAD~N..HEAD)"
                echo ""
                echo "Without arguments, walks first-parent chain from HEAD"
                echo "to the nearest merge commit and uses that as the diff base."
                echo "Errors if HEAD itself is a merge commit."
              }

              if [[ $# -gt 0 ]]; then
                case "$1" in
                  -h|--help)
                    usage
                    exit 0
                    ;;
                  *)
                    if [[ "$1" =~ ^[0-9]+$ ]]; then
                      revs="HEAD~$1..HEAD"
                    elif [[ "$1" == *".."* ]]; then
                      revs="$1"
                    else
                      echo "Error: argument must be a revision range or a number, got: $1" >&2
                      usage
                      exit 1
                    fi
                    ;;
                esac
                shift
              else
                # Auto-detect: walk first-parent chain to nearest merge commit
                head_sha=$(git rev-parse HEAD)
                merge_base=$(git rev-list --first-parent --merges -n1 HEAD 2>/dev/null || true)

                if [[ -z "$merge_base" ]]; then
                  echo "No merge commit found in first-parent history, falling back to HEAD~1..HEAD" >&2
                  revs="HEAD~1..HEAD"
                elif [[ "$merge_base" == "$head_sha" ]]; then
                  echo "Error: HEAD is a merge commit. Specify a rev range explicitly." >&2
                  exit 1
                else
                  revs="$merge_base..HEAD"
                fi
              fi

              if [[ ! -d "$PATCHCRITIC_DIR" ]]; then
                echo "Error: patchcritic not found at $PATCHCRITIC_DIR" >&2
                echo "Run 'bentos-setup git-patchcritic' to clone it first." >&2
                exit 1
              fi

              echo "Running patchcritic on revs=$revs..." >&2

              exec perl \
                -Ilib \
                -Idist/dev/bcritical/policies/lib \
                -I"$PATCHCRITIC_DIR/lib" \
                "$PATCHCRITIC_DIR/bin/patchcritic" \
                --profile=.perlcriticrc \
                --debug=diff \
                --revs="$revs" \
                "$@"
            '';
          })
          (
            (pkgs.writeShellApplication {
              name = "glab";
              runtimeInputs = [ pkgs.glab ];
              text = ''
                export HTTP_PROXY=http://127.0.0.1:18080
                export HTTPS_PROXY=http://127.0.0.1:18080
                if [ -f "$HOME/.mitmproxy/mitmproxy-ca-cert.pem" ]; then
                  if ! cmp -s "$HOME/.mitmproxy/mitmproxy-ca-cert.pem" /etc/pki/tls/certs/mitmproxy-ca-cert.pem 2>/dev/null; then
                    sudo cp "$HOME/.mitmproxy/mitmproxy-ca-cert.pem" /etc/pki/tls/certs/
                  fi
                fi
                exec ${lib.getExe pkgs.glab} "$@"
              '';
            }).overrideAttrs
            (_: {
              name = "cursed-glab";
            })
          )
          git
        ]);
    };
}
