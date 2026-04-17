{
  self,
  config,
  lib,
  ...
}:
let
  flakeConfig = config;
  selfLib = self.lib.self;
in
{
  clan.inventory.instances.binarin-user = {
    module.name = "users";
    roles.default.tags.all = { };
    roles.default.settings = {
      user = "binarin";
      groups = [
        "wheel"
      ];
    };
    # roles.default.extraModules = self.nixosModules.binarin-baseline;
  };

  clan.inventory.instances.binarin-admin = {
    module.name = "admin";
    roles.default.tags.all = { };
    roles.default.settings = {
      allowedKeys =
        with lib;
        pipe (import "${self}/inventory/public-keys.nix") [
          (filterAttrs (
            _:
            {
              secure ? null,
              ...
            }:
            secure == true
          ))
          (mapAttrs (_: { public_key, ... }: public_key))
        ];
    };
  };

  flake.nixosModules.binarin-baseline =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.binarin-baseline";

      imports = [
        self.nixosModules.home-manager
        self.nixosModules.impermanence
        self.nixosModules.sops
        self.nixosModules.nix
        self.modules.generic.public-keys
      ];

      # Define sops secret for user-binarin-age
      # This gets decrypted using SSH host key (configured by sops module)
      # and made available to home-manager sops module
      sops.secrets.user-binarin-age =
        lib.mkIf (builtins.pathExists "${self}/secrets/${config.networking.hostName}/user-binarin-age")
          {
            sopsFile = selfLib.file' "secrets/${config.networking.hostName}/user-binarin-age";
            format = "binary";
            owner = "binarin";
            group = "binarin";
            mode = "0400";
          };

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
            "nix-access-tokens"
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
        self.homeModules.binarin-zsh
      ];

      programs.btop.enable = true;
      programs.jq.enable = true;
      programs.lsd.enable = true;
      programs.ripgrep.enable = true;

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

          set-option -s extended-keys on
          set-option -s extended-keys-format csi-u
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

      # this will build share/info/dir, emacs wants it
      programs.info.enable = true;

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
    };
}
