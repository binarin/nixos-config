{
  self,
  inputs,
  ...
}:
{
  flake-file.inputs = {
    nix-index-database.url = "github:nix-community/nix-index-database";
  };

  flake.nixosModules.interactive-cli =
    { pkgs, config, ... }:
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
        self.modules.generic.inventory-legacy
      ];

      options = {
        programs.doggo.enable = lib.mkEnableOption "Install `doggo` (`dig` replacement)";
      };

      config = lib.mkMerge [
        {

          programs.btop.enable = true;
          programs.darcs.enable = true;
          programs.doggo.enable = true;

          programs.fzf = {
            enable = true;
            defaultCommand = "fd";
            fileWidgetOptions = [ "--preview '${fzf_show_file_or_dir_preview}'" ];
            changeDirWidgetOptions = [ "--preview 'lsd --tree --color=always {} | head -200'" ];
          };

          programs.helix.enable = true;

          programs.htop.enable = true;

          programs.jq.enable = true;

          programs.lsd.enable = true;

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

          programs.ssh = {
            enable = true;
            enableDefaultConfig = false;
            matchBlocks = {
              mail = {
                match = ''
                  host mail.lynx-lizard.ts.net,mail,${config.inventory.ipAllocation.mail.home.primary.address}
                '';
                extraOptions = {
                  ControlMaster = "auto";
                  HostKeyAlias = "mail.lynx-lizard.ts.net";
                };
              };
              "*" = {
                extraOptions = {
                  ForwardAgent = "no";
                  AddKeysToAgent = "no";
                  Compression = "no";
                  ServerAliveInterval = "0";
                  ServerAliveCountMax = "3";
                  HashKnownHosts = "no";
                  UserKnownHostsFile = "~/.ssh/known_hosts";
                  ControlMaster = "no";
                  ControlPath = "~/.ssh/master-%r@%k:%p";
                  ControlPersist = "no";
                };
              };
            };
          };

          services.ssh-agent.enable = pkgs.stdenv.isLinux;

          programs.zellij = {
            enable = true;
          };

          programs.zsh = {
            shellAliases = {
              o = ''xdg-open'';
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
            (git-crypt.overrideAttrs (_prev: {
              patches = [
                (pkgs.fetchpatch {
                  url = "https://github.com/AGWA/git-crypt/commit/2da5e0016e53aba381046063c24c07f1bee3d824.diff";
                  sha256 = "sha256-fyHS2oeElUh+KEtvfnpf2/IiJPNSu03af+ilYFm3wOU";
                })
              ];
            }))
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
            ripgrep
            socat
            sops
            ssh-to-age
            sshfs
            tcpdump
            # trezor-agent
            unrar
            unzip
            viddy
            wget
            which
            whois
            yubikey-manager
            zip
          ];
        }
        (lib.mkIf config.programs.atuin.enable {
          home.sessionVariables.ATUIN_NOBIND = "1"; # XXX why I did it?
        })
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
