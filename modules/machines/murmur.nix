{
  self,
  inputs,
  lib,
  ...
}:
{
  flake-file.inputs.home-manager-unstable = {
    url = "github:nix-community/home-manager/master";
    inputs.nixpkgs.follows = "nixpkgs-unstable";
  };

  flake-file.inputs.nixgl = {
    url = "github:nix-community/nixgl";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  imports = [
    inputs.home-manager.flakeModules.home-manager
  ];

  flake.homeConfigurations.murmur = inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
    modules = [
      self.homeModules.murmur-home-allebedev
    ];

    extraSpecialArgs = {
      osConfig.services.graphical-desktop.enable = true;
      osConfig.impermanence.enable = false;
      self'.packages = self.packages.x86_64-linux;

      inputs' =
        with lib;
        pipe inputs [
          (lib.filterAttrs (_: v: v ? packages))
          (lib.mapAttrs (
            _: v: {
              packages = v.packages.x86_64-linux;
            }
          ))
        ];
    };
  };

  flake.homeModules.murmur-home-allebedev =
    {
      lib,
      pkgs,
      config,
      inputs',
      ...
    }:
    {
      key = "nixos-config.modules.home.murmur-home-allebedev";
      imports = [
        self.homeModules.emacs
        self.homeModules.foot
        self.homeModules.firefox
        self.homeModules.niri
        self.homeModules.direnv
        self.homeModules.xdg-autostart
        self.homeModules.binarin-baseline
        self.homeModules.binarin-ssh
        self.homeModules.standalone-home-manager-zsh
      ];

      nixpkgs = {
        config.allowUnfree = true;
        overlays = [
          inputs.emacs-overlay.overlays.default
          inputs.nixgl.overlay
          self.overlays.my-emacs
          self.overlays.waybar-org-clock
          self.overlays.sicstus-manual
          self.overlays.lan-mouse
          inputs.niri.overlays.default
          inputs.nix-ai-tools.overlays.shared-nixpkgs
          (final: prev: {
            # bubblewrap = final.writeShellScriptBin "bwrap" ''
            #   exec /usr/bin/bwrap "$@"
            # '';
            swaylock = final.writeShellScriptBin "swaylock" ''
              exec /usr/bin/swaylock "$@"
            '';
            slack =
              let
                slackWrapper = final.writeShellScriptBin "slack" ''
                  ${lib.getExe prev.slack} --no-sandbox "$@"
                '';
              in

              final.buildEnv {
                name = "slack";
                ignoreCollisions = true;
                paths = with prev; [
                  slackWrapper
                  slack
                ];
                meta.mainProgram = "slack";
              };

            nix = inputs.determinate.packages.x86_64-linux.default.overrideAttrs {
              meta.mainProgram = "nix";
            };
          })
        ];
      };

      services.ssh-agent.enable = true;
      home.packages =
        (with pkgs; [
          # age-plugin-yubikey
          lan-mouse
          geeqie
          age
          bpftrace
          btop
          chromium
          gimp
          google-chrome
          htop
          iftop
          iotop
          kanata
          mosh
          niri
          nixfmt
          nixgl.nixGLIntel
          passage
          rclone
          restic
          ripgrep
          slack
          wl-clipboard
          xwayland-satellite
          go_1_26
        ])
        ++ (with pkgs.llm-agents; [
          claude-code
          workmux
        ]);

      xdg.dataFile."applications/niri.desktop".text = ''
        [Desktop Entry]
        Name=Niri
        Comment=A scrollable-tiling Wayland compositor
        Exec=nixGLIntel niri-session
        Type=Application
        DesktopNames=niri
      '';

      xdg.dataFile."applications/slack.desktop".text = ''
        [Desktop Entry]
        Name=Slack
        Exec=${lib.getExe pkgs.slack} %U
        MimeType=x-scheme-handler/slack;
        Type=Application
        Icon=${pkgs.slack}/share/pixmaps/slack.png
      '';

      xdg.dataFile."applications/ssh-adb-k-b.desktop".text = ''
        [Desktop Entry]
        Name=ssh to adb.k.b
        Exec=foot-unique-window "SSH|adb.k.b" --override=colors.background=001800 -e ssh -t adb.k.b tmux -u new-session -A -D -s binarin
        Type=Application
        Terminal=false
        Categories=System;
        Icon=foot
      '';
      xdg.dataFile."applications/ssh-db-k-b.desktop".text = ''
        [Desktop Entry]
        Name=ssh to db.k.b
        Exec=foot-unique-window "SSH|db.k.b" --override=colors.background=001800 -e ssh -t db.k.b tmux -u new-session -A -D -s binarin
        Type=Application
        Terminal=false
        Categories=System;
        Icon=foot
      '';
      xdg.dataFile."applications/ssh-db3-k-b.desktop".text = ''
        [Desktop Entry]
        Name=ssh to db3.k.b
        Exec=foot-unique-window "SSH|db3.k.b" --override=colors.background=001800 -e ssh -t db3.k.b tmux -u new-session -A -D -s binarin
        Type=Application
        Terminal=false
        Categories=System;
        Icon=foot
      '';

      xdg.mimeApps.defaultApplications."x-scheme-handler/slack" = "slack.desktop";

      # xdg.autostart.override."remotesupport".notShownIn = [
      #   "niri"
      # ];

      xdg.autostart.override."nvidia-settings-autostart".notShownIn = [
        "niri"
      ];

      xdg.enable = true;
      xdg.mime.enable = true;
      xdg.mimeApps.enable = true;

      programs.git = {
        settings.user = {
          name = "Alexey Lebedeff";
          email = "binarin@binarin.info";
        };
      };

      dconf.enable = lib.mkForce false; # XXX no dbus or something

      nix.package = null;

      home.username = "allebedev";
      home.homeDirectory = "/home/allebedev";
      home.stateVersion = "25.11"; # Please read the comment before changing.
      programs.home-manager.enable = true;

      services.restic = {
        enable = true;
        backups = {
          localbackup = {
            paths = [
              "/home/allebedev/org"
              "/home/allebedev/.passage"
            ];
            repository = "/backup/local";
            passwordFile = "/home/allebedev/.config/restic/restic-password";
            initialize = true;
          };
          gdrive = {
            paths = [
              "/home/allebedev/org"
              "/home/allebedev/.passage"
            ];
            repository = "rclone:gdrive:";
            passwordFile = "/home/allebedev/.config/restic/restic-password";
            timerConfig = {
              OnCalendar = "10:20";
              RandomizedDelaySec = "5h";
            };
          };
        };
      };
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
    profiles.user = {
      path = self.lib.deploy-home-manager self.homeConfigurations.b-dev-kvm;
    };
  };

  flake.homeConfigurations.b-dev-kvm = inputs.home-manager-unstable.lib.homeManagerConfiguration {
    pkgs = inputs.nixpkgs-unstable.legacyPackages.x86_64-linux;

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
    { ... }:
    {
      key = "nixos-config.modules.home.standalone-home-manager-zsh";
      programs.zsh.envExtra = ''
        export BK_OTEL_ENABLED=false

        if [[ -f $HOME/.nix-profile/bin/zsh ]]; then
            export SHELL=$HOME/.nix-profile/bin/zsh
        fi

        if [[ -d $HOME/.local/bin && !( $PATH == *$HOME/.local/bin* ) ]]; then
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
      ];

      nixpkgs.config.allowUnfree = true;
      nixpkgs.overlays = [
        inputs.nix-ai-tools.overlays.shared-nixpkgs
      ];

      xdg.enable = false;
      programs.zsh.dotDir = config.home.homeDirectory;

      programs.zsh.envExtra = ''
        if [[ -n $SSH_AUTH_SOCK && -e $SSH_AUTH_SOCK && $SSH_AUTH_SOCK != *ssh-agent-stable.sock ]]; then
          ln -sf "$SSH_AUTH_SOCK" "$XDG_RUNTIME_DIR/ssh-agent-stable.sock"
        fi

        export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent-stable.sock"
      '';

      programs.zsh.initContent = ''
        if type -p bk > /dev/null ; then
          source <(bk completion zsh)
        fi
      '';

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

      home.packages = with pkgs.llm-agents; [
        claude-code
        workmux
      ];
    };

}
