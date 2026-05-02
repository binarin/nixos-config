{
  self,
  inputs,
  config,
  ...
}:
let
  flakeConfig = config;
in
{
  flake-file.inputs = {
    nix-index-database.url = "github:nix-community/nix-index-database";
  };

  flake.nixosModules.interactive-cli =
    {
      pkgs,
      config,
      self',
      ...
    }:
    {
      key = "nixos-config.modules.nixos.interactive-cli";

      programs.iftop.enable = true;
      programs.iotop.enable = true;
      programs.mosh.enable = true;

      programs.wireshark.enable = true;
      programs.wireshark.package =
        if config.services.graphical-desktop.enable then pkgs.wireshark-qt else pkgs.tshark;

      environment.systemPackages = with pkgs; [
        ripgrep

        # XXX review things below
        bridge-utils
        cryptsetup
        inotify-tools
        iptables
        nftables
        pciutils
        usbutils
      ];

    };

  flake.homeModules.interactive-cli =
    {
      self',
      lib,
      pkgs,
      config,
      osConfig,
      ...
    }:
    let
      fzf_show_file_or_dir_preview = "if [ -d {} ]; then lsd --tree --color=always {} | head -200; else bat -n --color=always --line-range :500 {}; fi";
    in
    {
      key = "nixos-config.modules.home.interactive-cli";

      imports = [
        inputs.nix-index-database.homeModules.nix-index
        self.homeModules.direnv
        self.homeModules.trezor-agent
        self.homeModules.binarin-ssh
      ];

      options = {
        programs.doggo.enable = lib.mkEnableOption "Install `doggo` (`dig` replacement)";
      };

      config = lib.mkMerge [
        {

          programs.aria2.enable = true;

          programs.darcs.enable = true;
          programs.doggo.enable = true;

          programs.fzf = {
            enable = true;
            defaultCommand = "fd";

            fileWidgetOptions = [ "--preview '${fzf_show_file_or_dir_preview}'" ];
            fileWidgetCommand = "fd --type f";

            changeDirWidgetOptions = [ "--preview 'lsd --tree --color=always {} | head -200'" ];
            changeDirWidgetCommand = "fd --type d";
          };

          programs.helix.enable = true;

          programs.htop.enable = true;

          programs.nix-index = {
            enable = true;
            enableZshIntegration = true;
          };

          programs.nix-index-database.comma.enable = true;

          programs.rtorrent = {
            enable = true;
            extraConfig = ''
              encoding.add=utf-8
            '';
          };

          services.ssh-agent.enable = pkgs.stdenv.isLinux;

          programs.zsh = {
            shellAliases = {
              o = "xdg-open";
              yolo = "claude --dangerously-skip-permissions --print";
            };
            dirHashes = {
              docs = config.xdg.userDirs.documents;
              dl = config.xdg.userDirs.download;
            };
          };

          programs.gh.enable = true;

          home.packages = with pkgs; [
            age
            ansible
            binutils # 'strings' mostly
            curl
            curlie
            deploy-rs
            docker-compose
            docker-credential-helpers
            doggo
            duf
            e2fsprogs
            elinks
            eternal-terminal
            file
            git-annex
            self'.packages.git-crypt-patched
            gnum4
            gnumake
            gnupg
            gron
            httpie
            inetutils
            ipcalc
            jless
            just
            lsof
            man-pages
            mc
            (lib.lowPrio moreutils) # `sponge` util; also includes `parallel`, but I want the `GNU parallel`, so lowPrio
            nil # nix language server
            nix-output-monitor
            nix-tree
            nmap
            ntfsprogs
            openssl
            ov
            p7zip
            parallel
            pv
            python3
            recode
            socat
            sops
            ssh-to-age
            sshfs
            tcpdump
            unrar
            unzip
            viddy
            wget
            which
            whois
            yubikey-manager
            zip
          ];

          programs.readline = {
            enable = true;
            extraConfig = ''
              # $if term=foot
              set show-mode-in-prompt on
              set emacs-mode-string "\1\e]133;A\e\\\2"
              $endif
            '';
          };
        }
        (lib.mkIf config.programs.fzf.enable {
          programs.doggo.enable = true;
          programs.fd.enable = true;
          programs.lsd.enable = true;
          programs.zsh.initContent = ''
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
        (lib.mkIf config.programs.starship.enable {
          home.sessionVariables = {
            STARSHIP_CACHE = lib.mkDefault "${config.xdg.cacheHome}/starship";
          };
        })
        (lib.mkIf config.programs.zsh.syntaxHighlighting.enable {
          programs.zsh.initContent = ''
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
        {
          programs.zsh.initContent = ''
            source ${
              self.packages.${pkgs.stdenv.hostPlatform.system}.zsh-autoquoter
            }/share/zsh/zsh-autoquoter/zsh-autoquoter.zsh
            ZAQ_PREFIXES+=('git commit( [^ ]##)# -[^ -]#m')
            ZAQ_PREFIXES+=('fjc')
            ZAQ_PREFIXES+=('yolo')
            ZSH_HIGHLIGHT_HIGHLIGHTERS+=(zaq)

            fjc() {
              local input="$1"
              local title body
              if [[ "$input" == *"|"* ]]; then
                title="''${input%%|*}"
                body="''${input#*|}"
              else
                title="$input"
                body=""
              fi
              fj issue create "$title" --body "$body"
            }
          '';
        }
        (lib.mkIf (osConfig.services.graphical-desktop.enable && pkgs.stdenv.isLinux) {
          home.packages = with pkgs; [ gparted ];
        })
        (lib.mkIf pkgs.stdenv.isLinux {
          home.packages = with pkgs; [
            btrfs-progs
            exfatprogs
            gdb
            parted
            psmisc
            reptyr
            sysstat
            wol
          ];

        })
      ];
    };

}
